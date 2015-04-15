import leon.lang._
import leon.lang.synthesis._
import leon.annotation._
/**
 * { require(this.isPackedSet)
 * } ensuring { res => ... }
 **/

object SplayTree {
  val minVal = BigInt(0)
  val maxVal = BigInt(100)
  
  sealed abstract class OptInt {
    def <(that:BigInt) = this match {
      case None => true
      case Some(x) => x < that
    }
    def <=(that:BigInt) = this match {
      case None => true
      case Some(x) => x <= that
    }
    def >(that:BigInt) = this match {
      case None => true
      case Some(x) => x > that
    }
    def >=(that:BigInt) = this match {
      case None => true
      case Some(x) => x >= that
    }
  }
  case class Some(i:BigInt) extends OptInt
  case object None extends OptInt

  abstract class Tree
  case class Node(l:Tree, v:BigInt, r:Tree) extends Tree
  case object Leaf extends Tree

  def add(tree: Tree, x:BigInt):Tree = {
    require(isSorted(tree) && x >= minVal && x <= maxVal)
    tree match {
      case Leaf => Node(Leaf, x, Leaf)
      case Node(l, v, r) if (x == v) => Leaf//tree //nothing to add if already exists
      case Node(l, v, r) if (x < v) => Leaf//Node(add(l, x), v, r)
      case Node(l, v, r) if (x > v) => Leaf//Node(l, v, add(r, x))
    }
  } ensuring {res => isSorted(res)}
  

  def remove(tree:Tree, x:BigInt):Tree = {
    require(isSorted(tree))
    // splay(tree, x) match {
    //   case Node(l,v,r) if (v == x) => join(l, r)
    //   case n => n //leaf or node whith v != x
    // }
    val s = split(tree, x)
    join(s._1, s._2)
  }

  def join(tree:Tree, that:Tree):Tree = {
    tree //TODO
  }

  def split(tree:Tree, x:BigInt):(Tree, Tree) = {
    require(isSorted(tree))
    splay(tree, x) match {
      case Node(l,v,r) if (x == v) => (l, r)
      case n => (n, Leaf) //leaf flags that the element was not found
    }
  }

  //to limit the size of the trees to examine
  def maxSize(tree:Tree, n:BigInt):Boolean = tree match {
    case Leaf => true
    case Node(r, v, l) => (n > 0) && maxSize(l, n-1) && maxSize(r, n-1)
  }

  def contains(tree:Tree, v:BigInt):Boolean = {
    require(isSorted(tree)) //work for range 1 -> 10
    tree match {
      case Leaf => false
      case Node(l, x, r) =>
        if (v == x) true
        else if (v < x) contains(l, v)
        else contains(r, v)
    }
  }

  //current implementation of isSorted
  def isSorted(tree:Tree):Boolean = {
    //isSortedOB(tree, None, None)
    isSortedBURec(tree).sorted
  }
  
  case class SResult(min:BigInt, sorted:Boolean, max:BigInt)
  
  //bottum-up "inductive"
  def isSortedBURec(tree:Tree):SResult = tree match {
    case Leaf => SResult(minVal, true, maxVal)
    case Node(l, v, r) =>
      val lx = isSortedBURec(l)
      val rx = isSortedBURec(r)
      SResult(lx.min, (lx.sorted && rx.sorted && (lx.min < v) && (v < rx.max)), rx.max)
  }

  //top down "recursive"  
  def isSortedTD(tree:Tree, min:OptInt, max:OptInt):Boolean = tree match {
    case Leaf => true
    case Node(l, vi, r) =>
      (min, max) match {
        case (None, None) => val v = Some(vi); isSortedTD(l, min, v) && isSortedTD(r, v, max)
        case (Some(mi), None) => val v = Some(vi); (vi > mi) && isSortedTD(l, min, v) && isSortedTD(r, v, max)
        case (None, Some(ma)) => val v = Some(vi); (vi < ma) && isSortedTD(l, min, v) && isSortedTD(r, v, max)
        case (Some(mi), Some(ma)) => val v = Some(vi); (vi > mi) && (vi < ma) && isSortedTD(l, min, v) && isSortedTD(r, v, max)
      }
  }

  def splay(tree:Tree, v:BigInt):Tree = {
    require(maxSize(tree, 5) && isSorted(tree)) //&& isSorted(tree, Int.MinValue, Int.MaxValue))
    tree match {
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
                  case _ => splay(ll, v) match {
                    case Node(lll, llx, llr) => Node(lll, llx, Node(llr, lx, Node(lr, x, r)))
                    //case else?
                  }
                }
              }
              else {
                lr match {
                  case Leaf => Node(ll, lx, Node(lr, x, r))
                  case _ => splay(lr, v) match {
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
                  case _ => splay(rl, v) match {
                    case Node(rll, rlx, rlr) => Node(Node(l, x, rll), rlx, Node(rlr, rx, rr))
                    //case else?
                  }
                }
              } else {
                rr match {
                  case Leaf => Node(Node(l,x,rl),rx,rr)
                  case _ => splay(rr, v) match {
                    case Node(rrl, rrx, rrr) => Node(Node(Node(l, x, rl), rx, rrl), rrx, rrr)
                    //case else?
                  }
                }
              }
          }
        }

    }
  } //ensuring {res => isSorted(res, Int.MinValue, Int.MaxValue)}

  def zig = {
    require (true)
  }

  def zigzig = {}

  def zigzag = {}

  // def getMinAndRemove:(Int, Tree) = {
  //   require(this match {case n:Node => true case Leaf => false}) //can't remove anything if Leaf
  //   this match { //or splayMin
  //     case Node(Leaf, v, r) => (v, r)
  //     case Node(l, v, r) => l.getMinAndRemove match {case (vn, tn) => (vn, Node(tn, v, r))}
  //     case Leaf => (-1, this)
  //   }
  // }
}