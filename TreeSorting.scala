import TreeBasic._
import OptInt._

object TreeSorting {
  //current implementation of isSorted
  def isSorted(tree:Tree):Boolean = {
    //isSortedTD(tree, None, None)
    //isSortedBU(tree).sorted
    isSortedTriv(tree)
    //isSortedBuggy(tree)
  }

  //supposes that the tree is sorted!! might work if we apply the sorting algorithm to each node
  def maxTriv(tree: Tree): BigInt = {
    tree match {
      case Leaf => 0 //should never happen
      case Node(l,v,Leaf) => v
      case Node(l,v,r) => maxTriv(r)
    }
  }
  def minTriv(tree: Tree): BigInt = {
    tree match {
      case Leaf => 0 //should never happen
      case Node(Leaf,v,r) => v
      case Node(l,v,r) => minTriv(l)
    }
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
  
  //"inductively" induces total soring order
  def isSortedTriv(tree: Tree): Boolean = {
    tree match {
      case Leaf => true //should only match at root
      case Node(Leaf, v, Leaf) => true
      case Node(l@Node(_, vl, _), v, Leaf) => (vl < v) && (maxTriv(l) < v) && isSortedTriv(l)
      case Node(Leaf, v, r@Node(_, vr, _)) => (v < vr) && (v < minTriv(r)) && isSortedTriv(r)
      case Node(l@Node(_, vl, _),v,r@Node(_, vr, _)) => (vl < v) && (v < vr) && (maxTriv(l) < v) && (v < minTriv(r)) && isSortedTriv(l) && isSortedTriv(r)
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
}
