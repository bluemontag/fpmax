package igs.step2.smallrefactorings

import scala.io.StdIn._

object stdlib {

  trait Program[F[_]] {
    def finish[A](a: A): F[A]

    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

    def map[A, B](fa: F[A], ab: A => B): F[B]
  }

  object Program {
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](ab: A => B)(implicit F: Program[F]): F[B] = F.map(fa, ab)

    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
  }

  def finish[F[_], A](a: A)(implicit F: Program[F]): F[A] = F.finish(a)

  final case class IO[A](unsafeRun: () => A) {
    self =>
    final def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    final def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(self.unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)

    implicit val ProgramIO = new Program[IO] {
      def finish[A](a: A): IO[A] = IO.point(a)

      def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)

      def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
    }
  }

  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]

    def getStrLn: F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F

    implicit val ConsoleIO = new Console[IO] {
      def putStrLn(line: String): IO[Unit] = IO(() => println(line))

      def getStrLn: IO[String] = IO(() => readLine())
    }
  }

  def putStrLn[F[_] : Console](line: String): F[Unit] = Console[F].putStrLn(line)

  def getStrLn[F[_] : Console]: F[String] = Console[F].getStrLn

  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }

  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F

    implicit val RandomIO = new Random[IO] {
      def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
    }
  }

  def nextInt[F[_] : Random](upper: Int): F[Int] = Random[F].nextInt(upper)
}