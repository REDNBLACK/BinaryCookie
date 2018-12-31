package binarycookie

import java.lang.System.{lineSeparator => EOL}
import java.nio.{ByteBuffer, ByteOrder}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.{Failure, Try}

import cats.Show
import cats.data.State
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.string._

class ByteBuf(private val underlying: ByteBuffer) {
  import ByteBuf._

  def string: Pointer[String] =
    State { pos =>
      @tailrec
      def loop(supplier: => Byte, acc: Queue[Byte] = Queue.empty): String = supplier match {
        case 0 => new String(acc.toArray)
        case b => loop(supplier, acc :+ b)
      }

      pos -> loop(underlying.get)
    }

  def string(length: Int): Pointer[String] =
    slice(length).map(it => new String(it.underlying.array))

  def int: Pointer[Int] =
    as(4)(_.underlying.getInt(_))

  def double: Pointer[Double] =
    as(8)(_.underlying.getDouble(_))

  def read[A](fa: ByteBuf => Pointer[A], default: Boolean = true): Pointer[A] =
    int.flatMap { offset =>
      fa(bytes(offset, if (!default) underlying.array.length - offset else underlying.getInt(offset)))
    }

  def list[A](length: Int)(fa: => Pointer[A]): Pointer[List[A]] =
    (0 until length).toList.as(fa).sequence[Pointer, A]

  def skip(len: Int): Pointer[Unit] =
    slice(len).as(())

  def order(order: ByteOrder): Pointer[ByteBuf] =
    State(_ -> ByteBuf(underlying.order(order)))

  def slice(len: Int): Pointer[ByteBuf] =
    State(pos => pos + len -> bytes(pos, len))

  private def as[A](len: Int)(fa: (ByteBuf, Int) => A): Pointer[A] =
    State(pos => pos + len -> fa(this, pos))

  private def bytes(pos: Int, len: Int): ByteBuf = {
    val res = Array.fill[Byte](len)(127)

    Try(underlying.position(pos).asInstanceOf[ByteBuffer].get(res))
      .recoverWith { case e: Exception =>
        Failure(new Exception(s"Error at $pos:${pos + len} ($len) > ${underlying.array.length}", e))
      }
      .get

    ByteBuf(res)
  }
}
object ByteBuf {
  type Pointer[A] = State[Int, A]

  def apply(bytes: Array[Byte]): ByteBuf = apply(ByteBuffer.wrap(bytes))

  def apply(buf: ByteBuffer): ByteBuf = new ByteBuf(buf)
}

object Parser {
  import Domain._

  def rewindState[A](fa: => State[Int, A]): State[Int, A] =
    for {
      startState <- State.get[Int]
      _          <- State.set[Int](0)
      result     <- fa
      _          <- State.set[Int](startState)
    } yield result

  def parseCookie(buf: ByteBuf): State[Int, Cookie] =
    rewindState(
      for {
        _          <- buf.order(ByteOrder.LITTLE_ENDIAN)
        _          <- buf.skip(8)
        flags      <- buf.int
        _          <- buf.skip(4)
        url        <- buf.read(_.string, default = false)
        name       <- buf.read(_.string, default = false)
        path       <- buf.read(_.string, default = false)
        value      <- buf.read(_.string, default = false)
        _          <- buf.skip(8)
        expireDate <- buf.double.map(_.toLong).map(Instant.ofEpochSecond)
        createDate <- buf.double.map(_.toLong).map(Instant.ofEpochSecond)
      } yield Cookie(name, url, path, value, createDate, expireDate, Flags(flags))
    )

  def parsePage(buf: ByteBuf): State[Int, Page] =
    rewindState(
      for {
        _            <- buf.order(ByteOrder.LITTLE_ENDIAN)
        _            <- buf.skip(4)
        totalCookies <- buf.int
        cookies      <- buf.list(totalCookies)(buf.read(parseCookie))
        _            <- buf.skip(4)
      } yield Page(cookies)
    )

  def parse(buf: ByteBuf): State[Int, Pages] =
    for {
      _            <- buf.string(4)
      totalPages   <- buf.int
      pagesLength  <- buf.list(totalPages)(buf.int)
      pages        <- pagesLength.traverse(buf.slice(_) >>= parsePage)
    } yield Pages(pages)
}

object Domain {
  case class Flags(value: Int)
  object Flags {
    implicit val flagsShow: Show[Flags] = Show.show(_.value match {
      case 0 => ""
      case 1 => "Secure"
      case 4 => "HttpOnly"
      case 5 => "Secure; HttpOnly"
      case _ => "Unknown"
    })
  }

  case class Cookie(
      name: String,
      url: String,
      path: String,
      value: String,
      createDate: Instant,
      expireDate: Instant,
      flags: Flags
  )
  object Cookie {
    implicit val dateShow: Show[Instant] = new Show[Instant] {
      // Mac epoch format starts from 2001-01-01
      private val macEpoch = 978307200
      private val pattern = DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss zzz").withZone(ZoneId.of("GMT"))

      def show(zdt: Instant): String = pattern.format(zdt.plusSeconds(macEpoch))
    }

    implicit val cookieShow: Show[Cookie] = Show.show { cookie =>
      import cookie._
      show"Cookie: $name=$value; domain=$url; path=$path; expires=$expireDate; $flags"
    }
  }

  case class Page(list: List[Cookie])
  object Page {
    implicit val pageShow: Show[Page] = (p: Page) => p.list.iterator.map(_.show).mkString(EOL)
  }

  case class Pages(list: List[Page])
  object Pages {
    implicit val pagesShow: Show[Pages] = (p: Pages) => p.list.iterator.map(_.show).mkString(EOL + EOL + EOL)
  }
}