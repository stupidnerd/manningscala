import DataStructures.Cons
import DataStructures.Nil
import DataStructures.List

object Main {
  def main(args: Array[String]): Unit = {
    val myList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(List.take(myList, 3))
  }
}
