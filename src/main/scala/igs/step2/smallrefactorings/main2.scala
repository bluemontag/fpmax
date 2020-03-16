package igs.step2.smallrefactorings

import stdlib._
import testing._
import scala.util.Try

object main2 {

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def checkAnswer[F[_] : Console](name: String, num: Int, guess: Int): F[Unit] =
    if (num == guess) putStrLn("You guessed right, " + name + "!")
    else putStrLn("You guessed wrong, " + name + "! The number was: " + num)

  def checkContinue[F[_] : Program : Console](name: String): F[Boolean] =
    for {
      _ <- putStrLn("Do you want to continue, " + name + "?")
      choice <- getStrLn.map(_.toLowerCase)
      cont <- if (choice == "y") finish(true)
      else if (choice == "n") finish(false)
      else checkContinue(name)
    } yield cont

  def gameLoop[F[_] : Program : Console : Random](name: String): F[Unit] =
    for {
      num <- nextInt(5).map(_ + 1)
      _ <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      guess <- getStrLn
      _ <- parseInt(guess).fold(
        putStrLn("That is not a valid selection, " + name + "!")
      )((guess: Int) => checkAnswer(name, num, guess))
      cont <- checkContinue(name)
      _ <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def main[F[_] : Program : Console : Random]: F[Unit] =
    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello, " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()

  def mainIO: IO[Unit] = main[IO]

  def mainTestIO: TestIO[Unit] = main[TestIO]

  val TestExample = TestData(
    input = "john" :: "1" :: "n" :: Nil,
    output = Nil,
    nums = 0 :: Nil)

  def main(args:Array[String]): Unit = {
    mainIO.unsafeRun()
  }

  def runTest = mainTestIO.eval(TestExample).showResults

  // main with sample test data commented
//  def main(args:Array[String]): Unit = {
//      println(runTest)
//  }
}