package exercises

/**
 * Implement the function tail for removing the first element of a List. Note that the
 * function takes constant time. What are different choices you could make in your
 * implementation if the List is Nil? We’ll return to this question in the next chapter.
 */

object ExThreeDTwoPThirtyFive extends App {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  object List:
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  end List

  val testBefore = Cons(1, Cons(2, Cons(3, Nil)))
  val testAfter = List.tail(testBefore)
  println(testAfter)
}
