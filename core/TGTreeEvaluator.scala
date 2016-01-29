import scala.collection.immutable

/**
  * Created by stylejy on 13/12/2015.
  */
//dependentDepth: Integer number shows depth from the root.
class TGTreeEvaluator(dependentPointer: TGUserFunctionList, info: InformationStructure) {
  val keywordList = new TGKeywordList

  def run: Unit = {
    evalExpression(info.body)
  }

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
        case "defn" => functionDefn(secondInput)
        case "if" => functionIf(secondInput)
        case "or" => functionOr(secondInput)
        case "=" => functionEqual(secondInput)
        case "+" => functionPlus(secondInput)
        case "-" => functionMinus(secondInput)
      }
    } else {
      println("Keyword doesn't match")
    }
  }

  def deriveValueFromVector(inputVector: immutable.Seq[Expr]): String = {
    var value = ""
    var count = 0
    for (i <- inputVector) {
      value = evalExpression(inputVector(count)).toString
      count = count + 1
    }
    return value
  }

  def functionDefn(inputArguments: Argument): Unit = {
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
              userFunctionList.keywordList += (functionName.toString -> new InformationStructure(functionName.toString, group(1), group(2), info.depth+1))
          }

          println("   " + functionName.toString + "'s parameter: " + evalExpression(userFunctionList.keywordList(functionName.toString).parameter))
          println("   " + functionName.toString + "'s body: " + userFunctionList.keywordList(functionName.toString).body)
          userFunctionList.evalBody(functionName.toString)
        }
        else {
          println("error: wrong structure")
        }
      }
    }
  }

  def functionIf(inputArguments: Argument): Unit = {
    evalExpression(inputArguments)
  }

  def functionOr(inputArguments: Argument): Unit = {
    evalExpression(inputArguments)
  }

  def functionEqual(inputArguments: Argument): Unit = {
    evalExpression(inputArguments)
  }

  def functionPlus(inputArguments: Argument): Unit = {
    evalExpression(inputArguments)
  }

  def functionMinus(inputArguments: Argument): Unit ={
    evalExpression(inputArguments)
  }
}
