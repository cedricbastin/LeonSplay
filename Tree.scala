//import leon._
import leon.lang._
import leon.lang.synthesis._
import leon.annotation._
//import leon.collection._

/**
 * { require(this.isPackedSet)
 * } ensuring { res => ... }
 **/

object SplayTree {
  val minVal = BigInt(0)
  val maxVal = BigInt(10000)
  
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
  
  // def lessThan(a: OptInt, b: OptInt):Boolean = (a,b) match {
  //   case (None, _) => true //good?
  //   case (_, None) => true //good?
  //   case (Some(aa), Some(bb)) => (aa < bb)
  // }
  
  // def greaterThan(a: OptInt, b: OptInt):Boolean = (a,b) match {
  //   case (None, _) => true
  //   case (_, None) => true
  //   case (Some(aa), Some(bb)) => (aa < bb)
  // }
  
  def max(a: OptInt, b: OptInt):OptInt = (a, b) match {
    case (None, _) => b
    case (_, None) => a
    case (Some(aa), Some(bb)) => if (aa > bb) a else b 
  }
  def min(a: OptInt, b: OptInt):OptInt = (a,b) match {
    case (None, _) => b
    case (_, None) => a
    case (Some(aa), Some(bb)) => if (aa < bb) a else b 
  }

  abstract class Tree
  case class Node(l:Tree, v:BigInt, r:Tree) extends Tree
  case object Leaf extends Tree

  def add(tree: Tree, x:BigInt):Tree = {
    require(isSorted(tree))// && x > minVal && x < maxVal)
    tree match {
      case Leaf => Node(Leaf, x, Leaf)
      case Node(l, v, r) if (x == v) => tree //nothing to add if already exists
      case Node(l, v, r) if (x < v) => Node(add(l, x), v, r)
      case Node(l, v, r) if (x > v) => Node(l, v, add(r, x))
    }
  } ensuring {res => contains(res, x) && (content(tree)++Set(x) == content(res)) && isSorted(res)} //
  

  // def remove(tree:Tree, x:BigInt):Tree = {
  //   require(isSorted(tree))
  //   val s = split(tree, x)
  //   join(s._1, s._2)
  // } ensuring {res => !contains(res, x)}//isSorted(res)}

  // def join(tree:Tree, that:Tree):Tree = {
  //   tree //TODO
  // }

  // def split(tree:Tree, x:BigInt):(Tree, Tree) = {
  //   require(isSorted(tree))
  //   splay(tree, x) match {
  //     case Node(l,v,r) if (x == v) => (l, r)
  //     case n => (n, Leaf) //leaf flags that the element was not found
  //   }
  // }

  //to limit the size of the trees to examine
  def maxSize(tree:Tree, n:BigInt):Boolean = tree match {
    case Leaf => true
    case Node(r, v, l) => (n > 0) && maxSize(l, n-1) && maxSize(r, n-1)
  }

  def contains(tree:Tree, v:BigInt):Boolean = {
    //require(isSorted(tree)) //work for range 1 -> 10
    tree match {
      case Leaf => false
      case Node(l, x, r) =>
        if (v == x) true
        else if (v < x) contains(l, v)
        else contains(r, v)
    }
  }

  def content(t: Tree) : Set[BigInt] = t match {
    case Leaf => Set.empty
    case Node(l, v, r) => content(l) ++ Set(v) ++ content(r)
  }

  //current implementation of isSorted
  def isSorted(tree:Tree):Boolean = {
    //isSortedOB(tree, None, None)
    //isSortedBURec(tree).sorted
    //isSortedTriv(tree)
    isSortedBuggy(tree)
  }
  
  def isSortedBuggy(tree:Tree): Boolean = {
    tree match {
      case Leaf => true
      case Node(Leaf,               v, Leaf) => true
      case Node(l@Node(ll, vl, rl), v, Leaf) => (vl < v && isSortedBuggy(l))
      case Node(Leaf,               v, r@Node(lr, vr, rr)) => (v > vr && isSortedBuggy(r))
      case Node(l@Node(ll, vl, rl), v, r@Node(lr, vr, rr)) => (vl < v && v > vr && isSortedBuggy(l) && isSortedBuggy(r))
    }
  }
  
  //supposes that the tree is sorted!!
  def maxTriv(tree: Tree): OptInt = tree match {
    case Leaf => None
    case Node(l,v,Leaf) => Some(v)
    case Node(l,v,r) => maxTriv(r)
  }
  
  def minTriv(tree: Tree): OptInt = tree match {
    case Leaf => None
    case Node(Leaf,v,r) => Some(v)
    case Node(l,v,r) => minTriv(l)
  }
  
  def isSortedTriv(tree:Tree): Boolean = {
    tree match {
      case Leaf => true
      case Node(l,v,r) =>
        if (!(maxTriv(l) < v)) false
        else if (!(minTriv(r) > v)) false
        else isSortedTriv(l) && isSortedTriv(r) //lazy evaluation?
    }
  }
  
  case class SResult(min:OptInt, sorted:Boolean, max:OptInt)
  
  //bottum-up "inductive"
  def isSortedBURec(tree:Tree):SResult = tree match {
    case Leaf => SResult(None, true, None)
    case Node(l, v, r) =>
      val lx = isSortedBURec(l)
      if (!lx.sorted)
        SResult(None, false, None) //propagate false early
      else {
        val rx = isSortedBURec(r)
        if (!rx.sorted)
          SResult(None, false, None) //propagate false early
        else
          SResult(lx.min, (lx.sorted && rx.sorted && (lx.min < v) && (rx.max > v)), rx.max)
      }
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