object Main {
  def main(args: Array[String]): Unit = {
    val myList = DataStructures.List(1, 1, 1, 1, 2, 3)
    val filledList = DataStructures.List.fill(10, 5)
    val tail = DataStructures.List.tail(filledList)
    val lstNewHead = DataStructures.List.setHead(3, filledList)
    val droppedLst = DataStructures.List.drop(filledList, 2)
    val dropWhileLst = DataStructures.List.dropWhile[Int](myList, x => x == 1)
    println(myList)
    println(dropWhileLst)
    println(DataStructures.List.length(myList))
    println(DataStructures.List.length(dropWhileLst))
    println(DataStructures.List.init(myList))
  }
}
