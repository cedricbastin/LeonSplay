import TreeBasic._
import TreeSorting._

object TreeOps {
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
