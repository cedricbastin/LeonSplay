import leon.lang._
import leon.lang.synthesis._
import leon.annotation._
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
    
    def contains(v: Int):Boolean = {
      require(isBinSearchTree())
      this match {
        case Leaf => false
        case Node(l, x, r) =>
          if (v == x) true
          else if (v < x) l.contains(v)
          else r.contains(v)
      }
    }
    
    def isBinSearchTree(min:Int = Int.MinValue, max:Int = Int.MaxValue):Boolean = this match {
      case Leaf => true
      case Node(l,v,r) =>
        (v > min) && (v < max) && l.isBinSearchTree(min, v) && r.isBinSearchTree(v, max)
    }
    
    def add(x:Int):Tree = this.addBinary(x).splay(x)
    def addBinary(x:Int):Tree = this match {
      case Leaf => Node(Leaf, x, Leaf)
      case n@Node(l, v, r) if (x == v) => n
      case Node(l, v, r) if (x < v) => Node(l.addBinary(x), v, r)
      case Node(l, v, r) if (x > v) => Node(l, v, r.addBinary(x))
    } //ensuring { res => res.isBinSearchTree() }
    
    def remove(x:Int):Tree = {
      this
    }
    
    def join(that:Tree):Tree = {
      this
    }
    
    def split(x:Int):(Tree, Tree) = {
      require(contains(x))
      splay(x) match {
        case Leaf => (Leaf, Leaf) //should never happen
        case Node(l, v, r) if (v == x) => (Node(l, v, Leaf), r)
        case Node(l, v, r) => (l, Node(Leaf, v, r)) //if splay returned parent
      }
    }
    
    def splay(v:Int):Tree = {
      require(contains(v) && isBinSearchTree(Int.MinValue, Int.MaxValue))
      this match {
        case Leaf => Leaf //by definition
        case Node(l, x, r) if (v == x) => this //already at root
        case Node(l, x, r) if (v < x )
        //l zig:
        case Node(Node(a, x, b), p, c) if (v == x) => Node(a, x, Node(b, p, c))
        //l zig-zig:
        case Node(Node(Node(a, x, b), p, c), g, d) if (v == x) => Node(a, x, Node(b, p, Node(c, g, d)))
        //l zig-zag:
        case Node(Node(a, p, Node(b, x, c)), g, d) if (v == x) => Node(Node(a, p, b), x, Node(c, g, d))
        case Node(l, x, r) =>
          if (v < x) {
            l.
          } else { //(x < v)
            
          }
      }
    }
    
    // def splayMax:Tree = { //tree with empty left child
    //   this match {
    //     case Leaf => Leaf //nothing to splay?
    //     case Node(l, v, Leaf) => this //max found
    //     case Node(l, v, r) =>
    //       rn = r.splayMax
    //   }
    // } require (_.l == Leaf)

    // def getMinAndRemove:(Int, Tree) = {
    //   require(this match {case n:Node => true case Leaf => false}) //can't remove anything if Leaf
    //   this match { //or splayMin
    //     case Node(Leaf, v, r) => (v, r)
    //     case Node(l, v, r) => l.getMinAndRemove match {case (vn, tn) => (vn, Node(tn, v, r))}
    //     case Leaf => (-1, this)
    //   }
    // }
  }
  case class Node(l:Tree, v:Int, r:Tree) extends Tree
  case object Leaf extends Tree
}