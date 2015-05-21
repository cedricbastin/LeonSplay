import TreeBasic._
import TreeSorting._
import TreeSplay._

object TreeSplit {
  def splitPartial(tree:Tree, v:BigInt):(Tree, Tree) = {
    require(isSorted(tree) && contains(tree, v))
    splay(tree, v) match {
      case Node(l, x, r) if (x == v) => (l, r)
    }
  } ensuring { tup =>
    (content(tree) == content(tup._1)++content(tup._2)++Set(v))
  }
  
  def splitPartialNotContains(tree:Tree, v:BigInt):(Tree, Tree) = {
    require(isSorted(tree) && !contains(tree, v))
    splay(tree, v) match {
      case Leaf => (Leaf, Leaf)
      case Node(l, x, r) if (x < v) => (Node(l, x, Leaf), r)
      case Node(l, x, r) if (x > v) => (l, Node(Leaf, x, r))
    }
  } ensuring { tup =>
    (content(tree) == content(tup._1)++content(tup._2))
  }
  
  def trivial(tree:Tree, v:BigInt):BigInt = {
    require(
      tree match {
        case Node(_,x,_) => x == v
        case Leaf => false
      }
    )
    tree match {
      case Node(a,x,b) => x
    }
  } ensuring(res => res == v)
}
