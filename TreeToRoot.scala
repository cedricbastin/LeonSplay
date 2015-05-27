import TreeSorting._
import TreeSplay._
import TreeBasic._

object TreeToRoot {  
  def splayToRoot(tree:Tree, x:BigInt) = {
    require(isSorted(tree) && contains(tree, x) && !tree.isLeaf)
    splay(tree, x)
  } ensuring { res => res match {case Node(_, v, _) => (x == v)} }
}
