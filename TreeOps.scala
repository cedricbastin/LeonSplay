import TreeBasic._
import TreeSorting._
import TreeSplay._

object TreeOps {
   def addBin(tree: Tree, x:BigInt):Tree = {
     require(isSorted(tree))
     tree match {
       case Leaf => Node(Leaf, x, Leaf)
       case Node(l, v, r) if (x == v) => tree //value already exists
       case Node(l, v, r) if (x < v) => Node(addBin(l, x), v, r)
       case Node(l, v, r) if (x > v) => Node(l, v, addBin(r, x))
     }
   } ensuring {res =>
     (content(tree)++Set(x) == content(res)) //&& isSorted(res)
   }

   def rmBin(tree: Tree, x:BigInt):Tree = {
     tree //TODO
   }

   def join(r:Tree, l:Tree):Tree = {
     require(isSorted(r) && isSorted(l) && ((!r.isLeaf && !l.isLeaf && (maxTriv(l) < minTriv(r))) || (r.isLeaf || l.isLeaf)))
     (r, l) match {
       case (Leaf, _) => l
       case (_, Leaf) => r
       case _ =>
         splay(l, maxTriv(l)) match {
           case Node(t, x, Leaf) => Node(t, x, r)
           //case _ => l //match exhaustiveness cannot be proven here
         }
     }
   }
}
