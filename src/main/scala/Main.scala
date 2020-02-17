import DataStructures.Cons
import DataStructures.Nil
import DataStructures.List

object Main {
  def main(args: Array[String]): Unit = {
    val l1 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    println(List.flatten(l1))
  }
}
