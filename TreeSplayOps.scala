import TreeBasic._
import TreeSorting._
import TreeSplay._
import TreeOps._
import TreeSplit._

object TreeSplayOps {
  //2 possibilities to add a node
   def addSplay(tree: Tree, x:BigInt) = {
     require(isSorted(tree))
     splay(addBin(tree, x), x)
   } ensuring {res => contains(res, x)}

   def addSplit(tree:Tree, x:BigInt) = {
     require(isSorted(tree))
     val (l, r) = split(tree, x)
     Node(l, x, r)
   }

  //2 possibilities to remove a node
  def delete = {}
  // Deletion[edit]
// To delete a node x, use the same method as with a binary search tree: if x has two children,
//swap its value with that of either the rightmost node of its left sub tree (its in-order predecessor)
//or the leftmost node of its right subtree (its in-order successor). Then remove that node instead.
//In this way, deletion is reduced to the problem of removing a node with 0 or 1 children.
//Unlike a binary search tree, in a splay tree after deletion, we splay the parent of the removed node to
//the top of the tree.

  def deleteJoin(tree:Tree, x:BigInt):Tree = {
    require(isSorted(tree))
    splay(tree, x) match {
      case Leaf => Leaf
      case Node(l, v, r) if (v == x) => join(l,r)
      case tree => tree //did not contain x!
    }
  }
}
