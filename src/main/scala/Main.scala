import ListStructure.Cons
import ListStructure.Nil
import ListStructure.List
import BinaryTreeStructure.Tree
import BinaryTreeStructure.Branch
import BinaryTreeStructure.Leaf

object Main {
  def main(args: Array[String]): Unit = {
    val myTree = Branch(Branch(Leaf(2), Leaf(3)), Branch(Branch(Leaf(10), Leaf(3)), Leaf(4)))
    println(myTree)
    println(BinaryTreeStructure.size(myTree))
    println(BinaryTreeStructure.maximum(myTree))
    println(BinaryTreeStructure.depth(myTree))
    println(BinaryTreeStructure.map(myTree)(_ * 2))
    println(BinaryTreeStructure.fold(myTree, 0)(_ + _))
  }
}
