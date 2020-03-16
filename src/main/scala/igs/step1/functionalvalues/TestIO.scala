package igs.step1.functionalvalues

import ProgramIO._
import ConsoleIO._
import RandomIO._

case class TestData(input: List[String], output: List[String], nums: List[Int]) {

  def putStrLn(line: String): (TestData, Unit) = (copy(output = line :: output), ())

  def getStrLn: (TestData, String) = (copy(input = input.drop(1)), input.head)

  def nextInt(upper: Int) : (TestData, Int) = (copy(nums = nums.drop(1)), nums.head)

  def showResults = output.reverse.mkString("\n")
}

case class TestIO[A](run: TestData => (TestData, A)) { self =>

  def map[B](f: A => B): TestIO[B] = TestIO( t => self.run(t) match { case (t2,a) => (t2, f(a)) })

  def flatMap[B](f: A => TestIO[B]) : TestIO[B] = TestIO( t => self.run(t) match { case (t2, a) => f(a).run(t2)})

  def eval(t: TestData): TestData = run(t)._1
}

object TestIO {
  def point[A](a: => A): TestIO[A] = TestIO( t => (t, a))

  implicit val ProgramTestIO = new Program[TestIO] {
    def finish[A](a: => A): TestIO[A] = TestIO.point(a)

    def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] = fa.flatMap(afb)

    def map[A, B](fa: TestIO[A], ab: A => B): TestIO[B] = fa.map(ab)
  }
  implicit val ConsoleTestIO = new Console[TestIO] {
    def putStrLn(line: String): TestIO[Unit] = TestIO( t => t.putStrLn(line) )
    def getStrLn: TestIO[String] = TestIO( t => t.getStrLn )
  }
  implicit val RandomTestIO = new Random[TestIO] {
    def nextInt(upper: Int): TestIO[Int] = TestIO( t => t.nextInt(upper) )
  }
}
