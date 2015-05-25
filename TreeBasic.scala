object TreeBasic {

  sealed abstract class Tree {
    def isLeaf = this match {
      case Leaf => true
      case _ => false
    }
  }
  case class Node(l: Tree, v: BigInt, r: Tree) extends Tree
  case object Leaf extends Tree

  def contains(tree:Tree, v:BigInt):Boolean = {
    tree match {
      case Leaf => false
      case Node(l, x, r) => (v == x) || contains(l, v) || contains(r, v)
    }
  }

  def content(t: Tree): Set[BigInt] = t match {
    case Leaf => Set.empty
    case Node(l, v, r) => content(l) ++ Set(v) ++ content(r) //does order of concatenation influence efficiency?
  }
}
