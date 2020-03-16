package igs.step1.functionalvalues

import igs.step1.functionalvalues.ConsoleIO.Console
import igs.step1.functionalvalues.ProgramIO.Program
import igs.step1.functionalvalues.RandomIO.Random
import igs.step1.functionalvalues.ProgramIO.ProgramSyntax
import igs.step1.functionalvalues.UtilFunctions._

object Main {

  def checkContinue[F[_]: Program: Console](name: String): F[Boolean] =
    for {
      _     <- putStrLn("Do you want to continue, " + name + "?")
      input <- getStrLn.map(_.toLowerCase)
      cont  <- input match {
        case "y" => finish(true)
        case "n" => finish(false)
        case _   => checkContinue(name)
      }
    } yield cont

  def printResults[F[_]: Console](input: String, num: Int, name: String): F[Unit] =
    parseInt(input).fold(
      putStrLn("You did not enter a number")
    )(guess =>
      if (guess == num) putStrLn("You guessed right, " + name + "!")
      else putStrLn("You guessed wrong, " + name + "! The number was: " + num)
    )

  def gameLoop[F[_]: Program: Random: Console](name: String): F[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      input <- getStrLn
      _     <- printResults(input, num, name)
      cont  <- checkContinue(name)
      _     <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def main[F[_]: Program: Random: Console]: F[Unit] =
    for {
      _     <- putStrLn("What is your name?")
      name  <- getStrLn
      _     <- putStrLn("Hello, " + name + ", welcome to the game!")
      _     <- gameLoop(name)
    } yield ()

  def mainIO: IO[Unit] = main[IO]

  def main(args:Array[String]): Unit = {
    mainIO.unsafeRun()
  }

  def mainTestIO: TestIO[Unit] = main[TestIO]

  val TestExample =
    TestData(
      input  = "Ignacio" :: "1" :: "n" :: Nil,
      output = Nil,
      nums   = 0 :: Nil
    )

  def runTest = mainTestIO.eval(TestExample).showResults

  // main with sample test data commented
  /*def main(args:Array[String]): Unit = {
      println(runTest)
  }*/

}
