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
      require(isBinSearchTree(Int.MinValue, Int.MaxValue))
      this match {
        case Leaf => false
        case Node(l, x, r) =>
          if (v == x) true
          else if (v < x) l.contains(v)
          else r.contains(v)
      } ensuring(res.isBinSearchTree(Int.MinValue, Int.MaxValue))
    }
    
    def isBinSearchTree(min:Int, max:Int):Boolean = this match {
      case Leaf => true
      case Node(l, v, r) =>
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
      //require(contains(v) && isBinSearchTree(Int.MinValue, Int.MaxValue))
      this match {
        case Leaf => Leaf //nothing to splay
        case n @ Node(l, x, r) =>
          if (v == x) n
          else if (v < x) {
            l match {
              case Leaf => Node(l, x, r) //there is nothing smaller -> return parent
              case Node(ll, lx, lr) =>
                if (v == lx) Node(ll, v, Node(lr, x, r))
                else if (v < lx) {
                  ll match {
                    case Leaf => Node(ll, lx, Node(lr, x, r))
                    case _ => ll.splay(v) match {
                      case Node(lll, llx, llr) => Node(lll, llx, Node(llr, lx, Node(lr, x, r)))
                      //case else?
                    }
                  }
                }
                else {
                  lr match {
                    case Leaf => Node(ll, lx, Node(lr, x, r))
                    case _ => lr.splay(v) match {
                      case Node(lrl, lrx, lrr) => Node(Node(ll, lx, lrl), lrx, Node(lrr, x, r))
                      //case else?
                    }
                  }
                }
            }
          } else {
            r match {
              case Leaf => Node(l, x, r)
              case Node(rl, rx, rr) => 
                if (v == rx) {
                  Node(Node(l,x,rl), v, rr)
                } else if (v < rx) {
                  rl match {
                    case Leaf => Node(Node(l, x, rl), rx, rr)
                    case _ => rl.splay(v) match {
                      case Node(rll, rlx, rlr) => Node(Node(l, x, rll), rlx, Node(rlr, rx, rr))
                      //case else? 
                    }
                  }
                } else {
                  rr match {
                    case Leaf => Node(Node(l,x,rl),rx,rr)
                    case _ => rr.splay(v) match {
                      case Node(rrl, rrx, rrr) => Node(Node(Node(l, x, rl), rx, rrl), rrx, rrr)
                      //case else?
                    }
                  }
                }
            }
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