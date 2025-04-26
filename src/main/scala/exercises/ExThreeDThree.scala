package exercises

object ExThreeDThree {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A] (head: A, tail: List[A]) extends List[A]

  object List {
    def setHead[A](newHead: A, l: List[A]): List[A] = {
      Cons(newHead, l)
    }
  }

  @main def main(): Unit = {
    val test: List[Int] = Cons(2, Cons(3, Nil))
    val nt: List[Int] = List.setHead(1, test)

    println(test)
    println(nt)
  }
}
