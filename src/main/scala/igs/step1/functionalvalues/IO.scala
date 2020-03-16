package igs.step1.functionalvalues


case class IO[A](unsafeRun: () => A) {
  self =>
  def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun()).unsafeRun())

  def pure[A](a: A): IO[A] = IO(() => a)
}
