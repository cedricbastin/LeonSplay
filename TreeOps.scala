import TreeBasic._
import TreeSorting._

object TreeOps {
   def addBin(tree: Tree, x:BigInt):Tree = {
     require(isSorted(tree))
     tree match {
       case Leaf => Node(Leaf, x, Leaf)
       case Node(l, v, r) =>
         if (x == v) {tree}
         else if (x < v) {Node(addBin(l, x), v, r)}
         else {Node(l, v, addBin(r, x))}
     }
   } ensuring {res =>
     (content(tree)++Set(x) == content(res)) // && isSorted(res)
   }

   def rmBin(tree: Tree, x:BigInt):Tree = {
     require(isSorted(tree))
     tree match {
       case Leaf => Leaf //not contained
       case Node(l, v, r) if (v < x) => Node(l, v, rmBin(r, x))
       case Node(l, v, r) if (x < v) => Node(rmBin(l, x), v, r)
       case Node(Leaf, v, r) if (v == x) => r
       case Node(l, v, Leaf) if (v == x) => l
       case Node(l, v, r) if (v == x) =>
         maxToRoot(l) match {
           case Node(ll, max, Leaf) => Node(ll, max, r)
         }
     }
   } ensuring {res => content(res) == content(tree) -- Set(x)} //not removed if not contained

   def remContains(tree: Tree, x:BigInt):Tree = {
     require(isSorted(tree) && contains(tree, x))
     rmBin(tree, x)
   } ensuring {res => content(res) == content(tree) -- Set(x)}

    def remNotContains(tree: Tree, x:BigInt):Tree = {
     require(isSorted(tree) && !contains(tree, x))
     rmBin(tree, x)
   } ensuring {res => content(tree) == content(res)}
    
   def maxToRoot(tree:Tree):Tree = { //acts like splay(max) without balancing
     require(isSorted(tree))
     def rec(tree:Tree):(Tree, BigInt) = {
       require(isSorted(tree) && !tree.isLeaf)
       tree match {
         case Node(l, v, Leaf) => (l, v)
         case Node(l, v, r) =>
           val re = rec(tree)
           (Node(l, v, re._1), re._2)
       }
     } ensuring {res => (content(res._1)++Set(res._2) == content(tree))}
     tree match {
       case Leaf => Leaf
       case _ =>
         val res = rec(tree)
         Node(res._1, res._2, Leaf) //put maximum to leaf
     }
   } ensuring {res => (content(tree) == content(res)) && (res match {case Leaf => true case Node(_,_,Leaf) => true})}

   def join(r:Tree, l:Tree):Tree = {
     require(isSorted(r) && isSorted(l) && ((!r.isLeaf && !l.isLeaf && (maxTriv(l) < minTriv(r))) || (r.isLeaf || l.isLeaf)))
     (r, l) match {
       case (Leaf, _) => l
       case (_, Leaf) => r
       case _ =>
         maxToRoot(l) match {
           case Node(t, x, Leaf) => Node(t, x, r)
           //case _ => l //match exhaustiveness cannot be proven here
         }
     }
   } ensuring {res => content(res) == content(l) ++ content(r)}
}
