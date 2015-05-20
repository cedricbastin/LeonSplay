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
    isSortedBU(tree).sorted
  }
  
  //bottum-up "inductive"
  case class SResult(min:OptInt, sorted:Boolean, max:OptInt)
  def isSortedBU(tree:Tree):SResult = tree match {
    case Leaf => SResult(None, true, None) //should only apply to root node
    case Node(Leaf, v, Leaf) => SResult(Some(v), true, Some(v))
    case Node(Leaf, v, r) =>
      val rx = isSortedBU(r)
      SResult(Some(v), rx.sorted && rx.min > v, rx.max)
    case Node(l, v, Leaf) =>
      val lx = isSortedBU(l)
      SResult(lx.min, lx.sorted && lx.max < v, Some(v))
    case Node(l, v, r) =>
      val lx = isSortedBU(l)
      if (!lx.sorted)
        lx //propagate false early
      else {
        val rx = isSortedBU(r)
        if (!rx.sorted)
          rx //propagate false early
        else
          SResult(lx.min, (lx.sorted && rx.sorted && (lx.min < v) && (rx.max > v)), rx.max)
      }
  }
  
  def splay(tree:Tree, v:BigInt):Tree = {
    require(isSorted(tree) && contains(tree, v)) //&& isSorted(tree, Int.MinValue, Int.MaxValue))
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
  } ensuring {res => (content(tree) == content(res)) && (res match {case Node(_, x, _) => (x == v)}) }//isSorted(res)}

  
  def splayToRoot(tree:Tree, v:BigInt):BigInt = {
    require(isSorted(tree) && contains(tree, v))
    splay(tree, v) match {
      case Node(l, x, r) => x
    }
  } ensuring { res => res == v}
  
  //does not keep ordering
  // def moveToRoot(tree:Tree, v:BigInt):Tree = {
  //   require(isSorted(tree))
  //   tree match {
  //     case Leaf => Leaf
  //     case Node(l,x,r) if (x == v) => tree
  //     case Node(Leaf, x, r) if (x < v) => tree //parent
  //     case Node(l, x, Leaf) if (x > v) => tree //parent
  //     case Node(Node(ll,lx,lr),x,r) if (lx == v) => Node(Node(ll,x,lr),lx,r)
  //     case Node(l,x,Node(rl, rx, rr)) if (rx == v) => Node(l,rx,Node(rl, x, rr))
  //     case Node(l,x,r) if (v < x) => moveToRoot(l, v) match {case Node(ll,lx,lr) => Node(Node(ll,x,lr),lx,r)}
  //     case Node(l,x,r) if (x < v) => moveToRoot(r, v) match {case Node(rl, rx, rr) => Node(l,rx,Node(rl, x, rr))}
  //   }
  // }
}
