import TreeBasic._
import TreeSorting._

object TreeSplay {
  def splay(tree:Tree, v:BigInt):Tree = {
    require(isSorted(tree)) //&& contains(tree, v)
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
  } ensuring {res => (content(tree) == content(res)) }//&& (res match {case Leaf => true case Node(_, x, _) => if (contains(tree, v)) (x == v) else true})}//isSorted(res)}

  // def splay2(tree: Tree, v: BigInt):Tree = {
  //   require(isSorted(tree))
  //   tree match {
  //     case Leaf => Leaf
  //     case Node(l, x, r)                          if (x == v)         => tree //already root
  //     case Node(Leaf, x, r)                       if (v < x)          => tree //parent
  //     case Node(l, x, Leaf)                       if (x < v)          => tree //parent
  //     case Node(Leaf, x, Leaf)                                        => tree //nothing to splay
  //     //zig
  //     case Node(Node(a, x, b), p, c)              if (x == v)         => Node(a, x, Node(b, p, c)) //zig left -> dept 1
  //     case Node(Node(a@Leaf, x, b), p, c)         if (v < x)          => Node(a, x, Node(b, p, c))
  //     case Node(Node(a, x, b@Leaf), p, c)         if (x < v && v < p) => Node(a, x, Node(b, p, c))
  //     //zag
  //     case Node(c, p, Node(b, x, a))              if (x == v)         => Node(Node(c, p, b), x, a) //zag right
  //     case Node(c, p, Node(b, x, a@Leaf))         if (x < v)          => Node(Node(c, p, b), x, a)
  //     case Node(c, p, Node(b@Leaf, x, a))         if (p < v && v < x) => Node(Node(c, p, b), x, a)
  //     //zig-zig
  //     case Node(Node(Node(a, x, b), p, c), g, d)  if (x == v)         => Node(a, x, Node(b, p, Node(c, g, d))) //zig-zig left
  //     case Node(Node(xNode, p, c), g, d)          if (v < p)          => splay2(xNode, v) match {
  //       case Node(a, x, b)                                            => Node(a, x, Node(b, p, Node(c, g, d)))
  //     }
  //     case Node(Node(a, p, Node(b, x, c)), g, d)  if (x == v)         => Node(Node(a, p, b), x, Node(c, g, d)) //zig-zag left
  //     case Node(Node(a, p, xNode), g, d)          if (p < v && v < g) => splay2(xNode, v) match {
  //       case Node(b, x, c)                                            => Node(Node(a, p, b), x, Node(c, g, d))
  //     }
  //     case Node(d, g, Node(c, p, Node(b, x, a)))  if (x == v)         => Node(Node(Node(d, g, c), p, b), x, a) //zag-zag right
  //     case Node(d, g, Node(c, p, xNode))          if (p < v)          => splay2(xNode,v) match {
  //       case Node(b, x, a)                                            => Node(Node(Node(d, g, c), p, b), x, a)
  //     }
  //     case Node(d, g, Node(Node(c, x, b), p, a))  if (x == v)         => Node(Node(d, g, c), x, Node(b, p, a)) //zag-zig right
  //     case Node(d, g, Node(xNode, p, a))          if (g < v && v < p) => splay2(xNode, v) match {
  //       case Node(c, x, b)                                            => Node(Node(d, g, c), x, Node(b, p, a))
  //     }
  //   }
  // } ensuring {res => (content(tree) == content(res)) } //&& (res match {case Leaf => true case Node(_, x, _) => if (contains(tree, v)) (x == v) else true})
}
