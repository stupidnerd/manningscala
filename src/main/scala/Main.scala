import DataStructures.Cons
import DataStructures.Nil
import DataStructures.List

object Main {
  def main(args: Array[String]): Unit = {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    println(List.zipInt(l1, l2))
  }
}
