import scala.annotation.tailrec

object MathFun {
  def abs(n: Int): Int = {
    if (n < 0)
      -n
    else
      n
  }

  def formatAbs(n: Int): String = {
    val positive = abs(n)
    val msg: String = "Absolute value of %d is %d"
    msg.format(n, positive)
  }

  private def formatFactorial(n: Int): String = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n > 0) go(n - 1, acc * n)
      else
        acc
    }

    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst[T](array: Array[T], f: T => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= array.length) -1
      else if (f(array(n))) n
      else loop(n + 1)
    }

    loop(0)
  }
}
