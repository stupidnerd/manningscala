import MyListStructure.Cons
import MyListStructure.Nil
import MyListStructure.List

object Main {
  def main(args: Array[String]): Unit = {
    val myList = List(1, 2, 3, 4)
    val myList2 = List(4, 5)
    println(List.hasSubsequence(myList, myList2))
  }
}
