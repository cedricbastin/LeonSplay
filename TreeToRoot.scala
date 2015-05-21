import TreeSorting._
import TreeSplay._
import TreeBasic._

object TreeToRoot {  
  def splayToRoot(tree:Tree, v:BigInt):BigInt = {
    require(isSorted(tree) && contains(tree, v))
    splay(tree, v) match {
      case Node(l, x, r) => x
    }
  } ensuring { res => res == v}
}
