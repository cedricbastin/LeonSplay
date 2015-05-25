import TreeBasic._
import TreeSorting._
import TreeSplay._

object TreeSplit {
  def split(tree:Tree, x:BigInt):(Tree, Tree) = {
    require(isSorted(tree))
    splay(tree, x) match {
      case Leaf => (Leaf, Leaf)
      case Node(l, v, r) if (v <= x) => (Node(l, v, Leaf), r) //smaller or equal
      case Node(l, v, r) if (v > x) => (l, Node(Leaf, v, r))
    }
  } ensuring (res => content(tree) == content(res._1) ++ content(res._2))
}
