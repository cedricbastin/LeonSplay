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
  //limit the range for "easieræ checking
  //val minVal = BigInt(0)
  //val maxVal = BigInt(10000)
  
  //helper class to deal with infinite ranges
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
  case object None extends OptInt // represent -infinity and +infinity
  
  //helper functions
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

  //The main datastructure for the Splay Tree is the same than
  //we don't use the more complicated version with back-references to the parents
  abstract class Tree
  case class Node(l:Tree, v:BigInt, r:Tree) extends Tree
  case object Leaf extends Tree
  //could also have used: case class Leaf(v:BigInt)
  
  def addSplay(tree:Tree, x:BigInt):Tree = {
    require(isSorted(tree))
    splay(addBin(tree, x), x)
  } ensuring {res => contains(res, x) && (content(tree)++Set(x) == content(res)) && isSorted(res)} //
  
  def addBin(tree: Tree, x:BigInt):Tree = {
    require(isSorted(tree))// && x > minVal && x < maxVal)
    tree match {
      case Leaf => Node(Leaf, x, Leaf)
      case Node(l, v, r) if (x == v) => tree //nothing to add if already exists
      case Node(l, v, r) if (x < v) => Node(addBin(l, x), v, r)
      case Node(l, v, r) if (x > v) => Node(l, v, addBin(r, x))
    }
  } ensuring {res => contains(res, x) && (content(tree)++Set(x) == content(res)) && isSorted(res)}
  

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
    isSortedBURec(tree).sorted
    //isSortedTriv(tree)
    //isSortedBuggy(tree)
  }
  
  def isSortedBuggy(tree:Tree): Boolean = {
    tree match {
      case Leaf => true
      case Node(Leaf,               v, Leaf) => true
      case Node(l@Node(ll, vl, rl), v, Leaf) => (vl < v && isSortedBuggy(l))
      case Node(Leaf,               v, r@Node(lr, vr, rr)) => (v < vr && isSortedBuggy(r))
      case Node(l@Node(ll, vl, rl), v, r@Node(lr, vr, rr)) => (vl < v && v < vr && isSortedBuggy(l) && isSortedBuggy(r))
    }
  }
  
  //supposes that the tree is sorted!! might work if we apply the sorting algorithm to each node
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
  //also works on unsorted trees
  def maxVal(tree: Tree): OptInt = tree match {
    case Leaf => None
    case Node(l,v,r) => max(max(maxVal(l), Some(v)), maxVal(r))
  }
  def minVal(tree: Tree): OptInt = tree match {
    case Leaf => None
    case Node(l,v,r) => min(min(minVal(l), Some(v)), minVal(r))
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
    require(isSortedTriv(tree)) //works with "isSortedTriv"
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
  } ensuring {res => (content(tree) == content(res)) }//isSorted(res)}
  
  def splay2(tree: Tree, v: BigInt):Tree = {
    require(isSorted(tree))
    tree match {
      case Leaf => Leaf
      case Node(l, x, r)                          if (x == v)         => tree //already root
      case Node(Leaf, x, r)                       if (v < x)          => tree //parent
      case Node(l, x, Leaf)                       if (x < v)          => tree //parent
      //zig
      case Node(Node(a, x, b), p, c)              if (x == v)         => Node(a, x, Node(b, p, c)) //zig left
      case Node(Node(a@Leaf, x, b), p, c)         if (v < x)          => Node(a, x, Node(b, p, c))
      case Node(Node(a, x, b@Leaf), p, c)         if (x < v && v < p) => Node(a, x, Node(b, p, c))
      //zag
      case Node(c, p, Node(b, x, a))              if (x == v)         => Node(Node(c, p, b), x, a) //zag right
      case Node(c, p, Node(b, x, a@Leaf))         if (x < v)          => Node(Node(c, p, b), x, a)
      case Node(c, p, Node(b@Leaf, x, a))         if (p < v && v < x) => Node(Node(c, p, b), x, a)
      //zig-zig
      case Node(Node(Node(a, x, b), p, c), g, d)  if (x == v)         => Node(a, x, Node(b, p, Node(c, g, d))) //zig-zig left
      case Node(Node(xNode, p, c), g, d)          if (v < p)          => splay2(xNode, v) match {
        case Node(a, x, b)                                            => Node(a, x, Node(b, p, Node(c, g, d)))
      }
      case Node(Node(a, p, Node(b, x, c)), g, d)  if (x == v)         => Node(Node(a, p, b), x, Node(c, g, d)) //zig-zag left
      case Node(Node(a, p, xNode), g, d)          if (p < v && v < g) => splay(xNode, v) match {
        case Node(b, x, c)                                            => Node(Node(a, p, b), x, Node(c, g, d))
      }
      //left recursive
      //verify the following still!
      
      case Node(d, g, Node(c, p, Node(b, x, a)))  if (x == v)         => Node(Node(Node(d, g, c), p, b), x, a) //zag-zag right
      case Node(d, g, Node(c, p, xNode))          if (g < v && v < p) => splay2(xNode,v) match {
        case Node(b, x, a)                                            => Node(Node(Node(d, g, c), p, b), x, a)
      }
      case Node(d, g, Node(Node(c, x, b), p, a))  if (x == v)         => Node(Node(d, g, c), x, Node(b, p, a)) //zag-zig right
      case Node(d, g, Node(xNode, p, a))          if (p < v)          => splay2(xNode, v) match {
        case Node(c, x, b)                                            => Node(Node(d, g, c), x, Node(b, p, a))
      }
    }
  }

  //Tests
  //use argument:Nothing allows to chain several tests as block expressions are not allowed in Leon

}
