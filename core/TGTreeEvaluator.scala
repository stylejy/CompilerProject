import scala.collection.immutable

/**
  * Created by stylejy on 13/12/2015.
  */
//dependentDepth: Integer number shows depth from the root.
class TGTreeEvaluator {
  val keywordList = new TGKeywordList
  val indent = {
    /*var space = ""
    for (i <- 0 to info.depth) {
      space += "   "
    }
    space*/
    "   "
  }

  def evalExpression(expr: Expr, vTable: TGSymbolTable, fTable: TGSymbolTable): Any = {
    expr match {
      case Value(a) => a
      case Keyword(a) => a
      case IntNumber(a) => a.toInt
      case Sentence(a) => a match {
        case Value(a) => a
      }
      case Vector(a) => deriveValueFromVector(a)
      case Argument(a) => deriveValueFromVector(a)
      case Function(a, b) => evalFunction(a, b, vTable, fTable)
      case UserFunction(a, b) => evalUserFunction(a, b, vTable, fTable)

    }
  }

  def evalUserFunction(firstInput: Expr, secondInput: Argument, inputVTable: TGSymbolTable, inputFTable: TGSymbolTable): Any ={
    println(indent + "[CALL] User function: " + firstInput)
    //inputVTable.
  }

  def evalFunction(firstInput: Expr, secondInput: Argument, inputVTable: TGSymbolTable, inputFTable: TGSymbolTable ): Unit = {
    val keyword = evalExpression(firstInput, inputVTable, inputFTable).toString
    if(keywordList.keywordList(keyword)){
      //println(indent + info.name + "--> Keyword " + keyword +"\'s argument: " + secondInput)
      println("--> Keyword " + keyword +"\'s argument: " + secondInput)
      keyword match {
        case "defn" => functionDefn(secondInput, inputVTable, inputFTable)
        case "if" => functionIf(secondInput, inputVTable, inputFTable)
        case "or" => functionOr(secondInput, inputVTable, inputFTable)
        case "=" => functionEqual(secondInput, inputVTable, inputFTable)
        case "+" => functionPlus(secondInput, inputVTable, inputFTable)
        case "-" => functionMinus(secondInput, inputVTable, inputFTable)
      }
    } else {
      println("Keyword doesn't match")
    }
  }

  def deriveValueFromVector(inputVector: immutable.Seq[Expr]): String = {
    var value = ""
    var count = 0
    for (i <- inputVector) {
      value = inputVector(count) match {
        case Value(name) => name.toString
      }
      count = count + 1
    }
    return value
  }

  def functionDefn(inputArguments: Argument, inputVTable: TGSymbolTable, inputFTable: TGSymbolTable): Unit = {
    inputArguments match {
      case Argument(group) => {
        if(group.size == 3) {
          val parameter = group(1) match {
            case Vector(name) => deriveValueFromVector(name)
          }
          println(indent + " User declared function name: " + group(0))
          val functionName = group(0) match {
            case Value(name) => name.toString
          }
          try {
            inputFTable.list(functionName.toString)
          } catch {
            case ex: NoSuchElementException =>
              inputFTable.list += (functionName.toString -> group(2))

              try {
                inputVTable.list(parameter.toString)
              } catch {
                case ex: NoSuchElementException =>
                  inputVTable.list += (parameter.toString -> Empty(0))
              }
          }
          println(indent + " " + functionName.toString + "'s parameter: " + inputVTable.list(parameter.toString))
          println(indent + " " + functionName.toString + "'s body: " + inputFTable.list(functionName.toString))
          /* For test
          userFunctionList.evalBody(functionName.toString)
          */
        }
        else {
          println("error: wrong structure")
        }
      }
    }
  }

  def functionIf(inputArguments: Argument, inputVTable: TGSymbolTable, inputFTable: TGSymbolTable): Any = {
    inputArguments match {
      case Argument(group) => {
        if(group.size == 3) {
          println(indent + " if's conditional statement: " + group(0))
          println(indent + " if's true statement: " + group(1))
          println(indent + " if's false statement: " + group(2))

          if(group(0).equals(false))
            group(2)
          else
            group(1)
        }
      }
    }
  }

  def functionOr(inputArguments: Argument, inputVTable: TGSymbolTable, inputFTable: TGSymbolTable): Boolean = {
    var result = false
    inputArguments match {
      case Argument(group) => {
        for (i <- 0 until group.size) {
          println(indent + " or's condition(" + i + "): " + group(i))
          group(i) match {
            case Value(a) =>
              if(a.equals("true")) {
                println (true)
                return true
              }
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
        }
      }
    }
    println(result)
    result
  }

  def functionEqual(inputArguments: Argument, inputVTable: TGSymbolTable, inputFTable: TGSymbolTable): Boolean = {
    var result = false
    inputArguments match {
      case Argument(group) => {
        if(group.size == 2) {
          println(indent + " ='s first element: " + group(0))
          println(indent + " ='s second element: " + group(1))
          val value1 = group(0) match {
            case Value(a) => a
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
          val value2 = group(1) match {
            case Value(a) => a
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
          result = value1.equals(value2)
        }
      }
    }
    println(result)
    result
  }

  def functionPlus(inputArguments: Argument, inputVTable: TGSymbolTable, inputFTable: TGSymbolTable): Int = {
    var result = 0
    inputArguments match {
      case Argument(group) => {
        for (i <- 0 until group.size) {
          println(indent + " +'s element(" + i + "): " + group(i))
          group(i) match {
            case Value(a) =>
              val number = inputVTable.list(a) match {
                case IntNumber(a) => a.toInt
              }
              result += number
            case IntNumber(a) => result += a.toInt
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
        }
      }
    }
    println(result)
    result
  }

  def functionMinus(inputArguments: Argument, inputVTable: TGSymbolTable, inputFTable: TGSymbolTable): Int = {
    var result = 0
    inputArguments match {
      case Argument(group) => {
        for (i <- 0 until group.size) {
          println(indent + " -'s element(" + i + "): " + group(i))
          group(i) match {
            case Value(a) =>
              if(i == 0) {
                result = inputVTable.list(a) match {
                  case IntNumber(a) => a.toInt
                }
              } else {
                val number = inputVTable.list(a) match {
                  case IntNumber(a) => a.toInt
                }
                result -= number
              }
            case IntNumber(a) =>
              if(i == 0) {
                result = a.toInt
              } else {
                result -= a.toInt
              }
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
        }
      }
    }
    println(result)
    result
  }
}
