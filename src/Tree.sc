sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left,right) => 1 + size(right)
  }

  def maximum(treex : Tree[Int] ): Int = {
    val max_int = 0
    treex match {
      case Leaf(value) => value.max(max_int)
      case Branch(left,right) => maximum(left).max(maximum(right))
    }
  }
}