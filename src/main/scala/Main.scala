import DataStructures.Cons
import DataStructures.Nil
import DataStructures.List

object Main {
  def main(args: Array[String]): Unit = {
    val myList = List(1, 2, 3, 4)
    val myList2 = List(1, 2, 3)
    println(List.hasSubsequence(myList, myList2))
  }
}
