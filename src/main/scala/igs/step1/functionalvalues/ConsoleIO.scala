package igs.step1.functionalvalues

import scala.io.StdIn.readLine

object ConsoleIO {

  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]
    def getStrLn: F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  implicit val ConsoleIO: Console[IO] = new Console[IO] {
    def putStrLn(line: String): IO[Unit] = IO(() => println(line))
    def getStrLn: IO[String] = IO(() => readLine())
  }
}