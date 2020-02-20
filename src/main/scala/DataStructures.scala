import scala.annotation.tailrec

object DataStructures {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = {
      foldRight(ints, 0)(_ + _)
    }

    def sumFl(ints: List[Int]): Int = {
      foldLeft(ints, 0)(_ + _)
    }

    def product(ds: List[Double]): Double = {
      foldRight(ds, 1.0)(_ * _)
    }

    def productFl(ds: List[Double]): Double = {
      foldLeft(ds, 1.0)(_ * _)
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

    def take[A](lst: List[A], n: Int): List[A] = {
      if (n < 0) Nil
      else if (n == 0) lst

      @tailrec
      def loop(lst: List[A], n: Int, acc: List[A]): List[A] = {
        if (n == 0) acc
        else
          lst match {
            case Nil => acc
            case Cons(x, xs) => loop(xs, n - 1, Cons(x, acc))
          }
      }

      loop(lst, n, Nil)
    }


    @tailrec
    def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = lst match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x)) dropWhile(xs, f)
        else lst
    }

    @tailrec
    def dropWhile1[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
      case Cons(x, xs) if f(x) => dropWhile1(xs)(f)
      case _ => lst
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    def appendR[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((a, b) => Cons(a, b))

    def appendL[A](l1: List[A], l2: List[A]): List[A] = foldLeft(l1, l2)((a, b) => Cons(b, a))

    def length[A](lst: List[A]): Int = {
      foldRight(lst, 0)((_, b) => b + 1)
    }

    def lengthFl[A](lst: List[A]): Int = {
      foldLeft(lst, 0)((a, _) => a + 1)
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

    def foldRight[A, B](lst: List[A], z: B)(f: (A, B) => B): B =
      lst match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def foldLeft[A, B](lst: List[A], z: B)(f: (B, A) => B): B = lst match {
      case Nil => z
      case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
    }

    def reverse[A](lst: List[A]): List[A] = {
      foldRight(lst, Nil: List[A])((a, b) => append(b, Cons(a, Nil)))
    }

    // foldLeft through foldRight
    def foldLeftFR[A, B](lst: List[A], z: B)(f: (B, A) => B): B = foldRight(lst, z)((a, b) => f(b, a))

    // foldRight through foldLeft
    def foldRightFL[A, B](lst: List[A], z: B)(f: (A, B) => B): B = foldLeft(lst, z)((a, b) => f(b, a))

    def flatten[A](lst: List[List[A]]): List[A] = foldRight(lst, Nil: List[A])((a, b) => appendR(a, b))

    def increaseOne(lst: List[Int]): List[Int] = foldRight(lst, Nil: List[Int])((a, b) => Cons(a + 1, b))

    def doubleToString(lst: List[Double]): List[String] = foldRight(lst, Nil: List[String])((a, b) => Cons(a.toString, b))

    def map[A, B](lst: List[A])(f: A => B): List[B] = {
      foldRight(lst, Nil: List[B])((a, b) => Cons(f(a), b))
    }

    def filter[A](lst: List[A])(f: A => Boolean): List[A] = {
      foldRight(lst, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
    }

    def flatMap[A, B](lst: List[A])(f: A => List[B]): List[B] = {
      foldRight(lst, Nil: List[B])((a, b) => appendR(f(a), b))
    }

    def filterFM[A](lst: List[A])(f: A => Boolean): List[A] = {
      flatMap(lst)((a: A) => if (f(a)) Cons(a, Nil) else Nil)
    }

    def zipInt(l1: List[Int], l2: List[Int]): List[Int] = {
      def loop(l1: List[Int], l2: List[Int]): List[Int] = {
        l1 match {
          case Nil => Nil
          case Cons(x, xs) => Cons(x + head(l2), loop(xs, tail(l2)))
        }
      }

      loop(l1, l2)
    }

    def zipWith[A](l1: List[A], l2: List[A], f: (A, A) => A): List[A] = {
      def loop(l1: List[A], l2: List[A]): List[A] = {
        l1 match {
          case Nil => Nil
          case Cons(x, xs) => Cons(f(x, head(l2)), loop(xs, tail(l2)))
        }
      }

      loop(l1, l2)
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      throw new NotImplementedError()
    }
  }

}
