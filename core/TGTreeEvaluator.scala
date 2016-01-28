import scala.collection.immutable

/**
  * Created by stylejy on 13/12/2015.
  */
class TGTreeEvaluator {
  val keywordList = new TGKeywordList

  def evalExpression(expr: Expr): Any = {
    expr match {
      case Value(a) => a
      case Keyword(a) => a
      case Vector(a) => deriveValueFromVector(a)
      case Argument(a) => deriveValueFromVector(a)
      case Function(a, b) => evalFunction(a, b)
      case UserFunction(a, b) => {
        evalExpression(a)
        evalExpression(b)
      }
    }
  }

  def evalFunction(firstInput: Expr, secondInput: Argument): Unit = {
    val keyword = evalExpression(firstInput).toString
    if(keywordList.keywordList(keyword)){
      println(" Keyword " + keyword +"\'s argument: " + secondInput)
      keyword match {
        case "defn" => {
          defn(secondInput)
        }
      }
    } else {
      println("Keyword doesn't match")
    }
  }


  /*def unGroupFromArgument(inputGroup: Any, returnNumber: Int): Unit = {
    val group: immutable.Seq[Expr] = inputGroup
    return group(returnNumber-1)
  }*/


  def deriveValueFromVector(inputVector: immutable.Seq[Expr]): String = {
    var value = ""
    var count = 0;
    for (i <- inputVector) {
      value = evalExpression(inputVector(count)).toString
      count = count + 1
    }
    return value
  }

  def defn(inputArguments: Argument): Unit = {
    inputArguments match {
      case Argument(group) => {
        if(group.size == 3) {
          val userFunctionList = new TGUserFunctionList

          println("   User declared function name: " + evalExpression(group(0)))
          val functionName = evalExpression(group(0))
          try {
            userFunctionList.keywordList(functionName.toString)
          } catch {
            case ex: NoSuchElementException =>
              userFunctionList.keywordList += (functionName.toString -> true)
          }

          println("   The function's parameter: " + evalExpression(group(1)))
          println("   The function's body: " + group(2))
        }
        else {
          println("error: wrong structure")
        }
      }
    }
  }
}
