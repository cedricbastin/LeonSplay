object OptInt {
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
}
