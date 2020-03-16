package igs.step1.functionalvalues

object ProgramIO {

  trait Program[F[_]] {
    def finish[A](a: => A): F[A]
    def chain[A, B](fa:F[A], f: A => F[B]): F[B]
    def map[A, B](fa:F[A], f: A => B): F[B]
  }

  object Program {
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A] ) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)
    def flatMap[B](f: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, f)
  }

  def pure[A](a: => A): IO[A] = IO( () => a)

  implicit val ProgramIO: Program[IO] = new Program[IO] {
    override def finish[A](a: => A): IO[A] = pure(a)

    override def chain[A, B](fa: IO[A], f: A => IO[B]): IO[B] = fa.flatMap(f)

    override def map[A, B](fa: IO[A], f: A => B): IO[B] = fa.map(f)
  }

}
