object BinaryTreeStructure {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](lead: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = {
    def loop[B](t: Tree[B], acc: Int): Int = t match {
      case Leaf(_) => 1
      case Branch(lead, right) => 1 + loop(lead, acc) + loop(right, acc)
    }

    loop(tree, 0)
  }
}
