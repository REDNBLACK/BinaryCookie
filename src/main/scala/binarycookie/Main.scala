package binarycookie

import java.nio.file.{Files, Paths}
import cats.instances.int._
import cats.syntax.show._

object Main extends App {
  args.headOption match {
    case Some(path) =>
      val buf = ByteBuf(Files.readAllBytes(Paths.get(path)))
      println(Parser.parse(buf).runEmpty.value._2.show)
    case _          => System.err.println("Usage: BinaryCookie pathToCookie")
  }
}
