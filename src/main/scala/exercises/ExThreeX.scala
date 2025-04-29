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

/**
 * Ex 3.9
 * Compute the length of a list using foldRight.
 * def length[A](as: List[A]): Int
 */

/**
 * Ex 3.10
 * Our implementation of foldRight is not tail-recursive and will result in a StackOver-
 * flowError for large lists (we say it’s not stack-safe). Convince yourself that this is the
 * case, and then write another general list-recursion function, foldLeft, that is
 * tail-recursive, using the techniques we discussed in the previous chapter. Here is its
 * signature:
 * def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B
 */

/**
 * Ex 3.11
 * Write sum, product, and a function to compute the length of a list using foldLeft.
 */

/**
 * Ex 3.12
 * Write a function that returns the reverse of a list (given List(1,2,3) it returns
 * List(3,2,1)). See if you can write it using a fold.
 */

/**
 * Implement append in terms of either foldLeft or foldRight
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

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
      @tailrec
      def loop(l: List[A], acc: B): B = l match {
        case Nil => acc
        case Cons(h, Nil) => f(acc, h)
        case Cons(h, t) => loop(t, f(acc, h))
      }
      loop(as, z)
    }

    def sumFL(l: List[Int]): Int = foldLeft(l, 0)((x,y) => x+y)
    def producrFL(l: List[Double]): Double = foldLeft(l, 1.0)(_*_)
    def lenFL[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

    def revers[A](l: List[A]): List[A] =
      @tailrec
      def loop(orig: List[A], res: List[A]): List[A] = orig match {
        case Nil => res
        case Cons(h, t) => loop(t, Cons(h, res))
      }
      loop(l, Nil)

      def append[T](l: List[T], el: T): List[T] = l match {
        case Nil => Cons(el, Nil)
        case Cons(h, Nil) => Cons(h, Cons(el, Nil))
        case Cons(h,t) => Cons(h, append(t, el))
      }
  }

  val example: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  println(List.drop(example, 3))

  println(List.dropWhile(example, (x: Int) => x < 3))

  println("3.6 warning")
  println(List.init(Cons(1, Cons(2, Nil))))
  println(List.init(example))

  println("3.8 warning")
  println(List.foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int]) (Cons(_,_)))

  println("3.9 warning")
  println(List.length(example))
  println(List.length(Cons(Nil, Nil)))
  println(List.length(Cons(1, Nil)))

  println("3.10-11 warning")
  println(List.producrFL(Cons(5.0, Cons(2.0, Nil))))
  println(List.sumFL(example))
  println(List.lenFL(Cons(1, Cons(2, Nil))))
  println(List.lenFL(example))

  println("3.12 warning")
  println(List.revers(example))
  
  println("3.14 warning")
  println(List.append(example, Cons(6, Nil)))
}