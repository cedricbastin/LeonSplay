
/**
 * { require(this.isPackedSet)
 * } ensuring { res => ... }
**/

object SplayTree {
  abstract class Tree {
    
    def maxSize(n:Int):Boolean = this match {
      case Leaf => true
      case Node(r, v, l) => (n > 0) || (l.maxSize(n-1) && r.maxSize(n-1))
    }
    
    def isBinSearchTree(min:Int = Int.MinValue, max:Int = Int.MaxValue):Boolean = this match {
      case Leaf => true
      case Node(l,v,r) =>
        (v > min) && (v < max) && l.isBinSearchTree(min, v) && r.isBinSearchTree(v, max)
    }
    
    def add(x:Int):Tree = this match {
      case Leaf => Node(Leaf, x, Leaf)
      case n@Node(l, v, r) if (x == v) => n
      case Node(l, v, r) if (x < v) => Node(l.add(x), v, r)
      case Node(l, v, r) if (x > v) => Node(l, v, r.add(x))
    } ensuring { res => res.isBinSearchTree() }
    
    def remove(x:Int):Tree = {
      this
    }
    
    def join(that:Tree):Tree = {
      this
    }
    
    def split(x:Int):(Tree, Tree) = {
      (this, this)
    }
    
    def splay(x:Int):Tree = {
      require(isBinSearchTree(Int.MinValue, Int.MaxValue))
      this
    }

    def getMinAndRemove():(Int, Tree) = this match { //or splayMin
      require(this match {case n:Node => true case Leaf => false}) //can't remove anything if Leaf
      case Node(Leaf, v, r) =>
    }

    def getMaxAndRemove():Int = this match {

    }
  }
  case class Node(l:Tree, v:Int, r:Tree) extends Tree
  case object Leaf extends Tree
}