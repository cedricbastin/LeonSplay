object TreeBasic {

  sealed abstract class Tree {
    def isLeaf = this match {
      case Leaf => true
      case _ => false
    }
    def isRoot(x:BigInt) = this match {
      case Leaf => false
      case Node(_, v, _) => (x == v)
    }
  }
  case class Node(l: Tree, v: BigInt, r: Tree) extends Tree
  case object Leaf extends Tree

  def contains(tree:Tree, v:BigInt):Boolean = {
  	content(tree).contains(v)
    // tree match {
    //   case Leaf => false
    //   case Node(l, x, r) => (v == x) || contains(l, v) || contains(r, v)
    // }
  }

  def content(t: Tree): Set[BigInt] = t match {
    case Leaf => Set.empty
    case Node(l, v, r) => content(l) ++ Set(v) ++ content(r) //does order of concatenation influence efficiency?
  }
}
