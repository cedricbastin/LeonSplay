import TreeSorting._

object TreeBasic {

  abstract class Tree
  case class Node(l: Tree, v: BigInt, r: Tree) extends Tree
  case object Leaf extends Tree

  def contains(tree:Tree, v:BigInt):Boolean = {
    tree match {
      case Leaf => false
      case Node(l, x, r) => (v == x) || contains(l, v) || contains(r, v)
    }
  }

  def content(t: Tree) : Set[BigInt] = t match {
    case Leaf => Set.empty
    case Node(l, v, r) => content(l) ++ Set(v) ++ content(r) //does order of concatenation influence efficiency?
  }

   def add(tree: Tree, x:BigInt):Tree = {
     require(isSorted(tree)) //does just work with isSortedBU
     tree match {
       case Leaf => Node(Leaf, x, Leaf)
       case Node(l, v, r) if (x == v) => tree //value already exists
       case Node(l, v, r) if (x < v) => Node(add(l, x), v, r)
       case Node(l, v, r) if (x > v) => Node(l, v, add(r, x))
     }
   } ensuring {res => content(tree)++Set(x) == content(res)} //isSorted(res) && (content(tree)++Set(x) == content(res)) contains(res, x) &&
}
