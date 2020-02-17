import scala.annotation.tailrec

object DataStructures {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def fill[A](elem: A, n: Int): List[A] = {
      if (n <= 0) Nil
      else {
        @tailrec
        def loop(count: Int, acc: List[A]): List[A] = {
          if (count <= 0) Nil
          else if (count == 1) Cons(elem, acc)
          else loop(count - 1, Cons(elem, acc))
        }

        loop(n, Nil)
      }
    }

    def tail[A](lst: List[A]): List[A] = lst match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    def head[A](lst: List[A]): A = lst match {
      case Nil => throw new Exception
      case Cons(x, _) => x
    }

    def setHead[A](head: A, lst: List[A]): List[A] = lst match {
      case Nil => Nil
      case Cons(_, xs) => Cons(head, xs)
    }

    @tailrec
    def drop[A](lst: List[A], n: Int): List[A] = {
      if (n < 0) Nil
      else if (n == 0) lst
      else lst match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    @tailrec
    def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = lst match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x)) dropWhile(xs, f)
        else lst
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    def length[A](lst: List[A]): Int = lst match {
      case Nil => 0
      case Cons(_, xs) => 1 + length(xs)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, _) =>
        def loop(n: Int, lst: List[A]): List[A] = {
          if (n == 1) Nil
          else Cons(head(lst), loop(n - 1, tail(lst)))
        }

        loop(length(l), l)
    }
  }

}
