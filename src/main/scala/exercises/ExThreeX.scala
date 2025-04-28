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

object ExThreeX extends App{
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
        else loop(i + 1, List.tail(li))
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

        def init[A](l: List[A]): List[A] = l match {
          case Nil => Nil
          case Cons(h, t) => if t == Nil then Nil else Cons(h, init(t))
        }

        def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
          as match {
            case Nil => z
            case Cons(head, tail) => f(head, foldRight(tail, z)(f))
          }

        def sum(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)
        def product(ns: List[Double]): Double = foldRight(ns, 1.0) (_*_)

        def length[A](as: List[A]): Int =
          @tailrec
          def counter(i: Int, l: List[A]): Int =
            l match {
              case Nil => i
              case Cons(h, t) => if h == Nil then i else counter(i+1, t)
            }
          counter(0, as)
  }

  val example: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  println(List.drop(example, 3))

  println(List.dropWhile(example, (x: Int) => x < 3))

  println("3.6 warning")
  //println(List.init(example))
  println(List.init(Cons(1, Cons(2, Nil))))
  println(List.init(example))

  println("3.8 warning")
  println(List.foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int]) (Cons(_,_)))

  println("3.9 warning")
  println(List.length(example))
}
