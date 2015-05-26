import TreeBasic._
import TreeSorting._
import TreeSplay._
import TreeOps._
import TreeSplit._

object TreeSplayOps {
  def search(tree: Tree, x:BigInt):BigInt = { //return key or parent key?
    require(isSorted(tree))
    splay(tree, x) match {
      case Leaf => 0 //default value?
      case Node(_, v, _) => v
    }
  } //bigger or smaller than x any guarantees?

  //2 possibilities to add a node:
  def addSplay(tree: Tree, x:BigInt):Tree = {
    require(isSorted(tree))
    splay(addBin(tree, x), x)
  } ensuring {res => contains(res, x)}

  def addSplit(tree:Tree, x:BigInt):Tree = {
    require(isSorted(tree))
    if (contains(tree, x))
      tree
    else {
     val (l, r) = split(tree, x)
     Node(l, x, r)
    }
   } ensuring {res => contains(res, x)}

  //2 possibilities to remove a node:
  def delete(tree:Tree, x:BigInt):Tree = {
    require(isSorted(tree))
    splay(rmBin(tree, x), x) //splay the parent
  }

  def deleteJoin(tree:Tree, x:BigInt):Tree = {
    require(isSorted(tree))
    splay(tree, x) match {
      case Leaf => Leaf
      case Node(l, v, r) if (v == x) => join(l,r)
      case tree => tree //did not contain x!
    }
  }

  def split(tree: Tree, x:BigInt)     = splay(tree, x) match {case Node(l, v, r) => (Node(l, v, Leaf), r)
  def search(tree: Tree, x:BigInt)    = splay(tree, x)
  def addSplay(tree: Tree, x:BigInt)  = splay(binAdd(tree, x), x)
  def addSplit(tree:Tree, x:BigInt)   = split(tree, x) match {case (l, r) => Node(l, x, r)} 
  def delete(tree:Tree, x:BigInt)     = splay(rmBin(tree, x), x)
  def deleteJoin(tree:Tree, x:BigInt) = splay(tree, x) match {case Node(l, v, r) if (v == x) => join(l,r)}

}
