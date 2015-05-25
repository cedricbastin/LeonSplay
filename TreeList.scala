import TreeBasic._

object TreeList {
  sealed abstract class List
  case class Cons(head: BigInt, tail: List) extends List
  case object Nil extends List

  def append(l1: List, l2: List):List = l1 match {
    case Nil => l2
    case Cons(head, tail) => Cons(head, append(tail, l2))
  }

  def toList(t: Tree): List = t match {
    case Leaf => Nil
    case Node(l, v, r) => append(toList(l), Cons(v, toList(r)))
  }

  def listIsSorted(l:List):Boolean = l match {
    case Nil => true
    case Cons(head, tail) => tail match {
      case Nil => true
      case c@Cons(thead, ttail) => (head <= thead) && listIsSorted(c)
    }
  }

  def add(l:List, v:BigInt):List = {
    require(listIsSorted(l))
    l match {
      case Nil => Cons(v, Nil)
      case c@Cons(head, tail) if (head > v) => Cons(v, c)
      case Cons(head, tail) => Cons(head, add(tail, v))
    }
  } ensuring {res => listIsSorted(res)}
}
