import scala.collection.immutable

/**
  * Created by stylejy on 13/12/2015.
  */
class TreeEvaluator {
  //For testing
  def eval(expr: Expr): Unit =
    expr match {
      case Value(a) => println(a)
      case Keyword(a) => println(a)
      case Vector(a) => deriveVector(a)
      case Argument(a) => deriveVector(a)
      case Function(a, b) => {
        eval(a)
        eval(b)
      }
      case UserFunction(a, b) => {
        eval(a)
        eval(b)
      }
    }

  def deriveVector(inputVector: immutable.Seq[Expr]): Unit = {
    var count = 0;
    for(i <- inputVector) {
      eval(inputVector(count))
      count = count + 1
    }
  }
}
