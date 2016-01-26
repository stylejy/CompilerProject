import scala.collection.immutable

/**
  * Created by stylejy on 13/12/2015.
  */
class TGTreeEvaluator {
  val keywordList = new TGKeywordList

  def eval(expr: Expr): Any = {
    expr match {
      case Value(a) => a
      case Keyword(a) => a
      case Vector(a) => deriveVector(a)
      case Argument(a) => deriveVector(a)
      case Function(a, b) => {
        val keyword = eval(a).toString
        if(keywordList.keywordList(keyword)){

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

  def deriveVector(inputVector: immutable.Seq[Expr]): Unit = {
    var count = 0;
    for(i <- inputVector) {
      eval(inputVector(count))
      count = count + 1
    }
  }
}
