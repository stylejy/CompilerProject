import scala.collection.immutable

/**
  * Created by stylejy on 13/12/2015.
  */
//dependentDepth: Integer number shows depth from the root.
class TGTreeEvaluator(dependentPointer: TGUserFunctionList, info: InformationStructure) {
  val keywordList = new TGKeywordList
  val indent = {
    var space = ""
    for (i <- 0 to info.depth) {
      space += "   "
    }
    space
  }

  def run(): Unit = {
    evalExpression(info.body)
  }

  def evalExpression(expr: Expr): Any = {
    expr match {
      case Value(a) => a
      case Keyword(a) => a
      case Vector(a) => deriveValueFromVector(a)
      case Argument(a) => deriveValueFromVector(a)
      case Function(a, b) => evalFunction(a, b)
      case UserFunction(a, b) => evalUserFunction(a, b)

    }
  }

  def evalUserFunction(firstInput: Expr, secondInput: Argument): Unit ={
    println(indent + "[CALL] User function: " + firstInput)
    evalExpression(secondInput)
  }

  def evalFunction(firstInput: Expr, secondInput: Argument): Unit = {
    val keyword = evalExpression(firstInput).toString
    if(keywordList.keywordList(keyword)){
      println(indent + info.name + "--> Keyword " + keyword +"\'s argument: " + secondInput)
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

          println(indent + " User declared function name: " + group(0))
          val functionName = evalExpression(group(0))
          try {
            userFunctionList.keywordList(functionName.toString)
          } catch {
            case ex: NoSuchElementException =>
              userFunctionList.keywordList += (functionName.toString -> new InformationStructure(functionName.toString, group(1), group(2), info.depth+1))
          }

          println(indent + " " + functionName.toString + "'s parameter: " + userFunctionList.keywordList(functionName.toString).parameter)
          println(indent + " " + functionName.toString + "'s body: " + userFunctionList.keywordList(functionName.toString).body)
          userFunctionList.evalBody(functionName.toString)
        }
        else {
          println("error: wrong structure")
        }
      }
    }
  }

  def functionIf(inputArguments: Argument): Unit = {
    inputArguments match {
      case Argument(group) => {
        if(group.size == 3) {
          val userFunctionList = new TGUserFunctionList

          println(indent + " if's conditional statement: " + group(0))
          userFunctionList.keywordList += ("ifCondition" -> new InformationStructure("ifCondition", Empty(0), group(0), info.depth+1))
          userFunctionList.evalBody("ifCondition")

          println(indent + " if's true statement: " + group(1))
          userFunctionList.keywordList += ("ifTrue" -> new InformationStructure("ifTrue", Empty(0), group(1), info.depth+1))
          userFunctionList.evalBody("ifTrue")

          println(indent + " if's false statement: " + group(2))
          userFunctionList.keywordList += ("ifFalse" -> new InformationStructure("ifFalse", Empty(0), group(2), info.depth+1))
          userFunctionList.evalBody("ifFalse")
        }
      }
    }
  }

  def functionOr(inputArguments: Argument): Unit = {
    inputArguments match {
      case Argument(group) => {
        val userFunctionList = new TGUserFunctionList

        for (i <- 0 until group.size) {
          println(indent + " or's condition(" + i + "): " + group(i))
          userFunctionList.keywordList += ("or"+i -> new InformationStructure("or"+i, Empty(0), group(i), info.depth+1))
          userFunctionList.evalBody("or"+i)
        }
      }
    }
  }

  def functionEqual(inputArguments: Argument): Unit = {
    inputArguments match {
      case Argument(group) => {
        if(group.size == 2) {
          val userFunctionList = new TGUserFunctionList

          println(indent + " ='s first element: " + group(0))
          userFunctionList.keywordList += ("equal1" -> new InformationStructure("equal1", Empty(0), group(0), info.depth+1))
          userFunctionList.evalBody("equal1")

          println(indent + " ='s second element: " + group(1))
          userFunctionList.keywordList += ("equal2" -> new InformationStructure("equal2", Empty(0), group(1), info.depth+1))
          userFunctionList.evalBody("equal2")

        }
      }
    }
  }

  def functionPlus(inputArguments: Argument): Unit = {
    inputArguments match {
      case Argument(group) => {
        val userFunctionList = new TGUserFunctionList

        for (i <- 0 until group.size) {
          println(indent + " +'s element(" + i + "): " + group(i))
          userFunctionList.keywordList += ("plus"+i -> new InformationStructure("plus"+i, Empty(0), group(i), info.depth+1))
          userFunctionList.evalBody("plus"+i)
        }
      }
    }
  }

  def functionMinus(inputArguments: Argument): Unit = {
    inputArguments match {
      case Argument(group) => {
        val userFunctionList = new TGUserFunctionList

        for (i <- 0 until group.size) {
          println(indent + " -'s element(" + i + "): " + group(i))
          userFunctionList.keywordList += ("minus"+i -> new InformationStructure("minus"+i, Empty(0), group(i), info.depth+1))
          userFunctionList.evalBody("minus"+i)
        }
      }
    }
  }
}
