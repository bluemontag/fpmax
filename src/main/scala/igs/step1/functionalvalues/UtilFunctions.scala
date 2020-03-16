package igs.step1.functionalvalues

import igs.step1.functionalvalues.ConsoleIO.Console
import igs.step1.functionalvalues.ProgramIO.Program
import igs.step1.functionalvalues.RandomIO.Random

import scala.util.Try

object UtilFunctions {

  def putStrLn[F[_] : Console](line: String): F[Unit] = Console[F].putStrLn(line)
  def getStrLn[F[_] : Console]: F[String] = Console[F].getStrLn
  def finish[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)
  def nextInt[F[_]](upper: Int)(implicit F: Random[F]): F[Int] = Random[F].nextInt(upper)
  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

}
