import scala.collection.immutable

/**
  * Created by stylejy on 13/12/2015.
  */
class TGTreeEvaluator {
  val keywordList = new TGKeywordList

  def eval(expr: Expr): Unit = {
    expr match {
      case Vector(a) => deriveVector(a)
      case Argument(a) => deriveVector(a)
      case Function(a, b) => {
        val keyword = deriveValue(a)
        if(keywordList.keywordList(keyword)){
          println("found")
        } else {
          println("no")
        }
      }
      case UserFunction(a, b) => {
        eval(a)
        eval(b)
      }
    }
  }

  def deriveValue(expr: Expr): String = expr match {
    case Value(a) => return a
    case Keyword(a) => return a
  }

  def deriveVector(inputVector: immutable.Seq[Expr]): Unit = {
    var count = 0;
    for(i <- inputVector) {
      eval(inputVector(count))
      count = count + 1
    }
  }
}
