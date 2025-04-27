package exercises

import scala.annotation.tailrec

/**
 * Generalize tail to the function drop, which removes the first n elements from a list.
 * Note that this function takes time proportional only to the number of elements being
 * dropped—we don’t need to make a copy of the entire List.
 * def drop[A](l: List[A], n: Int): List[A]
 */

/**
 * Implement dropWhile, which removes elements from the List prefix as long as they
 * match a predicate.
 * def dropWhile[A](l: List[A], f: A => Boolean): List[A]
 */

/**
 * Not everything works out so nicely. Implement a function, init, that returns a List
 * consisting of all but the last element of a List. So, given List(1,2,3,4), init will
 * return List(1,2,3). Why can’t this function be implemented in constant time like
 * tail?
 * def init[A](l: List[A]): List[A]
 *
 */

object ExThreeTwoAndFourAndFiveAndSix extends App{
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

    @annotation.tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(head, tail) =>
          if f(head) then dropWhile(tail, f)
          else l
      }
    }

    def init[A](l: List[A]): List[A] = {

      @annotation.tailrec
      def findNil(li: List[A], i: Int): Int = li match {
        case Nil => i
        case _ => findNil(li, i+1)
      }

      val frontier: Int = findNil(l, 0) - 1

      @tailrec
      def builder(li: List[A], i: Int, n: Int): List[A] = {
        if i == n then li
        else li match {
          case Cons(h: A, _) => builder(Cons(h, Nil), i+1, n)
        }
      }

      builder(l, 0, frontier)
    }
  }

  val example: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  println(List.drop(example, 3))

  println(List.dropWhile(example, (x: Int) => x < 3))

  println("3.6 warning")
  println(List.init(example))
}
