package exercises

/**
 * Generalize tail to the function drop, which removes the first n elements from a list.
 * Note that this function takes time proportional only to the number of elements being
 * dropped—we don’t need to make a copy of the entire List.
 * def drop[A](l: List[A], n: Int): List[A]
 */

object ExThreeFour extends App{
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      @annotation.tailrec
      def loop(i: Int, li: List[A]): List[A] = {
        if n == i then li
        else loop(i+1, List.tail(li))
      }
      loop(0, l)
    }
  }

  val example: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  println(List.drop(example, 3))
}
