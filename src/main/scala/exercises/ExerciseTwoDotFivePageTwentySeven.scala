package exercises

/**
 *Implement the higher-order function that composes two functions.
 * def compose[A,B,C](f: B => C, g: A => B): A => C
 */

object ExerciseTwoDotFivePageTwentySeven extends App {
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  val incr = (x: Int) => x + 1

  val mult = (x: Int) => x * 2

  println(compose(mult, incr)(5))
}
