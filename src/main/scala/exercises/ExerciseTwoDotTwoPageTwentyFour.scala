package exercises

/**
 * Implement isSorted, which checks whether an Array[A] is sorted according to a
 * given comparison function:
 * def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
 *
 * I don't understand this shit completely, I guess I must use function `ordered` with n
 * and n + 1 elements, but I'm not sure
 */

object ExerciseTwoDotTwoPageTwentyFour {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n > as.length) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)
    }
    loop(0)
  }
}
