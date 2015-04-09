import leon.lang._
import leon.lang.synthesis._
import leon.annotation._
/**
 * { require(this.isPackedSet)
 * } ensuring { res => ... }
 **/

object SplayTree {
  sealed abstract class OptInt
  case class Some(i:Int) extends OptInt
  case object None extends OptInt

  abstract class Tree
  case class Node(l:Tree, v:Int, r:Tree) extends Tree
  case object Leaf extends Tree

  def add(tree: Tree, x:Int):Tree = tree match {
    case Leaf => Node(Leaf, x, Leaf)
    case n@Node(l, v, r) if (x == v) => n
    case Node(l, v, r) if (x < v) => Node(add(l, x), v, r)
    case Node(l, v, r) if (x > v) => Node(l, v, add(r, x))
  } //ensuring { res => res.isBinSearchTree() }

  def remove(tree:Tree, x:Int):Tree = {
    tree //TODO
  }

  def join(tree:Tree, that:Tree):Tree = {
    tree //TODO
  }

  def split(tree:Tree, x:Int):(Tree, Tree) = {
    // require(contains(x))
    // splay(x) match {
    //   case Leaf => (Leaf, Leaf) //should never happen
    //   case Node(l, v, r) if (v == x) => (Node(l, v, Leaf), r)
    //   case Node(l, v, r) => (l, Node(Leaf, v, r)) //if splay returned parent
    // }
    (tree, tree)
  }

  //to limit the size of the trees to examine
  def maxSize(tree:Tree, n:Int):Boolean = tree match {
    case Leaf => true
    case Node(r, v, l) => (n > 0) && maxSize(l, n-1) && maxSize(r, n-1)
  }

  def contains(tree:Tree, v: Int):Boolean = {
    require(isSorted(tree)) //work for range 1 -> 10
    tree match {
      case Leaf => false
      case Node(l, x, r) =>
        if (v == x) true
        else if (v < x) contains(l, v)
        else contains(r, v)
    }
  }

  //current impl
  def isSorted(tree:Tree):Boolean = isSortedOB(tree, None, None)

  // 2.Optional boundaries  
  def isSortedOB(tree:Tree, min:OptInt, max:OptInt):Boolean = tree match {
    case Leaf => true
    case Node(l, vi, r) =>
      (min, max) match {
        case (None, None) => val v = Some(vi); isSortedOB(l, min, v) && isSortedOB(r, v, max)
        case (Some(mi), None) => val v = Some(vi); (vi > mi) && isSortedOB(l, min, v) && isSortedOB(r, v, max)
        case (None, Some(ma)) => val v = Some(vi); (vi < ma) && isSortedOB(l, min, v) && isSortedOB(r, v, max)
        case (Some(mi), Some(ma)) => val v = Some(vi); (vi > mi) && (vi < ma) && isSortedOB(l, min, v) && isSortedOB(r, v, max)
      }
  }

  // 1.First Algo
  def isSortedBasic(tree:Tree,min:Int, max:Int):Boolean = tree match {
    case Leaf => true
    case Node(l, v, r) =>
      (v > min) && (v < max) && isSortedBasic(l, min, v) && isSortedBasic(r, v, max)
  }

  def splay(tree:Tree, v:Int):Tree = {
    require(maxSize(tree, 5) )//&& isSorted(tree, Int.MinValue, Int.MaxValue))
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