import TreeBasic._
import TreeSorting._
import TreeSplay._
import TreeOps._
import TreeSplit._

object TreeSplayOps {
  def search(tree: Tree, x:BigInt):BigInt = { //return key or parent key
    require(isSorted(tree))
    splay(tree, x) match { //returns key or parent
      case Leaf => 0 //default value?
      case Node(_, v, _) => v
    }
  } //bigger or smaller than x any guarantees?

  //2 possibilities to add a node:
  def addSplay(tree: Tree, x:BigInt):Tree = {
    require(isSorted(tree))
    splay(addBin(tree, x), x)
  } ensuring {res => content(res) == content(tree) ++ Set(x)}

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
  def rmSplay(tree:Tree, x:BigInt):Tree = {
    require(isSorted(tree))
    splay(rmBin(tree, x), x) //splay the parent
  } ensuring {res => content(res) == content(tree) -- Set(x)}

  // def deleteJoin(tree:Tree, x:BigInt):Tree = {
  //   require(isSorted(tree))
  //   splay(tree, x) match {
  //     case Leaf => Leaf
  //     case Node(l, v, r) if (v == x) => join(l,r)
  //     case tree => tree //did not contain x!
  //   }
  // } ensuring {res => content(res) == content(tree) -- Set(x)}
}
