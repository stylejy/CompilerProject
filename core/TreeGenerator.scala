import scala.collection.immutable

/**
  * Created by stylejy on 13/12/2015.
  */
class TreeGenerator {
  //For testing
  def eval(expr: Expr): Unit =
    expr match {
      case Keyword(a) => println(a)
      case Vector(a) => println(a)
      case Argument(a) => deriveVector(a)
      case Function(a, b) => eval(a)
    }

  def deriveVector(inputVector: immutable.Seq[Expr]): Unit = {
    for (i <- inputVector.toArray) {

    }
  }
}
