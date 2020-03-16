package igs.step3.internationalization

import stdlib._
import testing._

import scala.util.Try

object main3 {

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def checkAnswer[F[_]: Console](name: String, num: Int, guess: Int)(implicit lang: String): F[Unit] =
    if (num == guess) putStrLn(new ConsoleOut.YouGuessedRight(name))
    else putStrLn(new ConsoleOut.YouGuessedWrong(name, num))

  def checkContinue[F[_]: Program: Console](name: String)(implicit lang: String): F[Boolean] =
    for {
      _       <- putStrLn(new ConsoleOut.DoYouWantToContinue(name))
      choice  <- getStrLn.map(_.toLowerCase)
      cont    <- if (choice == "y") finish(true)
      else if (choice == "n") finish(false)
      else checkContinue(name)
    } yield cont

  def gameLoop[F[_]: Program: Console: Random](name: String)(implicit lang: String): F[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn(new ConsoleOut.PleaseGuess(name))
      guess <- getStrLn
      _     <- parseInt(guess).fold(
        putStrLn(new ConsoleOut.ThatIsNotValid(name))
      )((guess: Int) => checkAnswer(name, num, guess))
      cont  <- checkContinue(name)
      _     <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def main[F[_]: Program: Console: Random](implicit lang:String): F[Unit] =
    for {
      _     <- putStrLn(new ConsoleOut.WhatIsYourName())
      name  <- getStrLn
      _     <- putStrLn(new ConsoleOut.WelcomeToGame(name))
      _     <- gameLoop(name)
    } yield ()

  def mainIO(implicit lang: String): IO[Unit] = main[IO]

  def mainTestIO(implicit lang: String): TestIO[Unit] = main[TestIO]

  //defining the language ("en" for english or "es" for spanish available)
  implicit val lang: String = "es"

  //run on testdata
  def main(args:Array[String]):Unit = println(mainTestIO.eval(TestExample).showResults)

  //run the real game
  //def main(args:Array[String]):Unit = mainIO.unsafeRun()

  val TestExample = TestData(
    input   = "john" :: "1" :: "n" :: Nil,
    output  = Nil,
    nums    = 0 :: Nil)

}
