package igs.step3.internationalization

import stdlib._

object testing {

  case class TestData(input: List[String], output: List[ConsoleOut], nums: List[Int]) {
    def showResults = output.reverse.map (_.getStr).mkString ("\n")

    def nextInt: (TestData, Int) = (copy(nums = nums.drop(1)), nums.head)

    def putStrLn(line: ConsoleOut): (TestData, Unit) = (copy(output = line :: output), ())

    def getStrLn: (TestData, String) = (copy(input = input.drop(1)), input.head)
  }

  case class TestIO[A](run: TestData => (TestData, A)) { self =>
    def map[B](f: A => B): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => (t, f(a)) })

    def flatMap[B](f: A => TestIO[B]): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => f(a).run(t) })

    def eval(t: TestData): TestData = self.run(t)._1
  }
  object TestIO {
    def point[A](a: => A): TestIO[A] = TestIO(t => (t, a))

    implicit val RandomTestIO = new Random[TestIO] {
      def nextInt(upper: Int): TestIO[Int] =
        TestIO(t => t.nextInt)
    }
    implicit val ProgramTestIO = new Program[TestIO] {
      def finish[A](a: A): TestIO[A] = TestIO.point(a)

      def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] = fa.flatMap(afb)

      def map[A, B](fa: TestIO[A], ab: A => B): TestIO[B] = fa.map(ab)
    }
    implicit val ConsoleTestIO = new Console[TestIO] {
      def putStrLn(line: ConsoleOut): TestIO[Unit] =
        TestIO(t => t.putStrLn(line))
      def getStrLn: TestIO[String] =
        TestIO(t => t.getStrLn)
    }
  }

}
