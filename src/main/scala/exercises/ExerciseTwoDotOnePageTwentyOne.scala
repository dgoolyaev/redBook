package exercises

/**
 * Have:
 * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
 * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
 * previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
 * local tail-recursive function.
 * def fib(n: Int): Int
 * I guess, it means that fib(1) is 0, fib(2) is 1 etc. I will do this
 */

object ExerciseTwoDotOnePageTwentyOne extends App{
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(x: Int, current: Int, prev: Int): Int = {
      if (x == n) current
      else loop(x+1, current+prev ,current)
    }

    if (n <= 0) 0
    else loop(1, 0, 1)
  }
  
  // sorry iam noob at unit testing
  Console println fib(1)
  Console println fib(2)
  Console println fib(3)
  Console println fib(4)
  Console println fib(5)
  Console println fib(6)
  Console println fib(7)
  Console println fib(8)
  Console println fib(9)
  Console println fib(10)
}
