object BinaryTreeStructure {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](lead: Tree[A], right: Tree[A]) extends Tree[A]

}
