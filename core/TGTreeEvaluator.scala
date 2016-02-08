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

  def evalExpression(expr: Expr, vTable: TGVariableSymbolTable, fTable: TGFunctionSymbolTable): Any = {
    expr match {
      case Value(a) => evalExpression(vTable.list(a), vTable, fTable)
      case Keyword(a) => a
      case IntNumber(a) => a.toInt
      case Bool(a) => a.toBoolean
      case Sentence(a) => a match {
        case Value(a) => a
      }
      case Vector(a) => deriveValueFromVector(a)
      case Argument(a) => deriveValueFromVector(a)
      case Function(a, b) => evalFunction(a, b, vTable, fTable)
      case UserFunction(a, b) => evalUserFunction(a, b, vTable, fTable)

    }
  }

  def evalUserFunction(firstInput: Expr, secondInput: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Any ={
    //println(indent + "[CALL] User function: " + firstInput)
    val initialArg = secondInput match {
      case Argument(a) => a
    }

    val arg = new Array[Expr](initialArg.size)
    for (i <- 0 until initialArg.size) {
      if(initialArg(i) match {
        case Function(a, b) => true
        case _ => false
      }) {
        arg(i) = initialArg(i) match {
          case Function(a, b) => IntNumber(evalFunction(a, b, inputVTable, inputFTable).toString)
        }
      } else {
        arg(i) = initialArg(i)
      }
    }

    //(+ (fib (- x 2)) (fib (- x 1))) x should be same, but if both fibs are shared vTable,
    //the first fib changes parameter value and the changed one will be used for the second fib.
    val newVTable = new TGVariableSymbolTable()
    for(i <- inputVTable.list) {
      newVTable.list += i
    }
    //newVTable.list.update("x",arg)
    val name = firstInput match {
      case Value(a) => a.toString
    }

    val vars = inputFTable.list(name)._1 match {
      case Vector(a) => a
    }

    if(arg.size.equals(vars.size)) {
      var counter = 0
      for(i <- vars) {
        val varName = i match {
          case Value(a) => a.toString
        }
        newVTable.list.update(varName, arg(counter))
        counter += 1
      }
    }
    evalExpression(inputFTable.list(name)._2, newVTable, inputFTable)
  }

  def evalFunction(firstInput: Expr, secondInput: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable ): Any = {
    val keyword = evalExpression(firstInput, inputVTable, inputFTable).toString
    if(keywordList.keywordList(keyword)){
      //println(indent + info.name + "--> Keyword " + keyword +"\'s argument: " + secondInput)
      //println("--> Keyword " + keyword +"\'s argument: " + secondInput)
      keyword match {
        case "defn" => functionDefn(secondInput, inputVTable, inputFTable)
        case "if" => functionIf(secondInput, inputVTable, inputFTable)
        case "or" => functionOr(secondInput, inputVTable, inputFTable)
        case "=" => functionEqual(secondInput, inputVTable, inputFTable)
        case "+" => functionPlus(secondInput, inputVTable, inputFTable)
        case "-" => functionMinus(secondInput, inputVTable, inputFTable)
        case "*" => functionMultiply(secondInput, inputVTable, inputFTable)
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

  def functionDefn(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Unit = {
    inputArguments match {
      case Argument(group) => {
        if(group.size == 3) {
          val parameter = group(1) match {
            case Vector(name) => deriveValueFromVector(name)
          }
          //println(indent + " User declared function name: " + group(0))
          val functionName = group(0) match {
            case Value(name) => name.toString
          }
          try {
            inputFTable.list(functionName.toString)
          } catch {
            case ex: NoSuchElementException =>
              inputFTable.list += (functionName.toString -> (group(1),group(2)))

              try {
                inputVTable.list(parameter.toString)
              } catch {
                case ex: NoSuchElementException =>
                  inputVTable.list += (parameter.toString -> Empty(0))
              }
          }
          //println(indent + " " + functionName.toString + "'s parameter: " + inputVTable.list(parameter.toString))
          //println(indent + " " + functionName.toString + "'s body: " + inputFTable.list(functionName.toString))
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

  def functionIf(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Any = {
    inputArguments match {
      case Argument(group) => {
        if (group.size == 3) {
          //println(indent + " if's conditional statement: " + group(0))
          //println(indent + " if's true statement: " + group(1))
          //println(indent + " if's false statement: " + group(2))

          if (evalExpression(group(0), inputVTable, inputFTable).toString.toBoolean)
            evalExpression(group(1), inputVTable, inputFTable)
          else
            evalExpression(group(2), inputVTable, inputFTable)
        }
      }
    }
  }

  def functionOr(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Boolean = {
    val result = false
    inputArguments match {
      case Argument(group) => {
        for (i <- 0 until group.size) {
          //println(indent + " or's condition(" + i + "): " + group(i))
          group(i) match {
            case Value(a) =>
              if(inputVTable.list(a).equals("true")) {
                //println(true)
                return true
              }
            case IntNumber(a) =>
              //println(true)
              return true
            case Bool(a) =>
              if(a.toBoolean) {
                //println(true)
                return true
              }
            case Function(a, b) =>
              if(evalFunction(a, b, inputVTable, inputFTable).equals(true)) {
                //println(true)
                return true
              }
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
        }
      }
    }
    //println(result)
    result
  }

  def functionEqual(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Boolean = {
    var result = false
    inputArguments match {
      case Argument(group) => {
        if(group.size == 2) {
          //println(indent + " ='s first element: " + group(0))
          //println(indent + " ='s second element: " + group(1))
          val value1 = group(0) match {
            case Value(a) => evalExpression(inputVTable.list(a), inputVTable, inputFTable)
            case IntNumber(a) => a.toInt
            case Sentence(a) => a.toString
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
          val value2 = group(1) match {
            case Value(a) => evalExpression(inputVTable.list(a), inputVTable, inputFTable)
            case IntNumber(a) => a.toInt
            case Sentence(a) => a.toString
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
          result = value1.equals(value2)
        }
      }
    }
    //println(result)
    result
  }

  def functionPlus(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Int = {
    var result = 0
    inputArguments match {
      case Argument(group) => {
        for (i <- 0 until group.size) {
          //println(indent + " +'s element(" + i + "): " + group(i))
          group(i) match {
            case Value(a) =>
              val number = inputVTable.list(a) match {
                case IntNumber(a) => a.toInt
              }
              result += number
            case IntNumber(a) => result += a.toInt
            case Function(a, b) =>
              result += evalFunction(a, b, inputVTable, inputFTable).toString.toInt
            case UserFunction(a, b) =>
              result += evalUserFunction(a, b, inputVTable, inputFTable).toString.toInt
          }
        }
      }
    }
    //println(result)
    result
  }

  def functionMinus(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Int = {
    var result = 0
    inputArguments match {
      case Argument(group) => {
        for (i <- 0 until group.size) {
          //println(indent + " -'s element(" + i + "): " + group(i))
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
            case Function(a, b) =>
              if(i == 0) {
                result = evalFunction(a, b, inputVTable, inputFTable).toString.toInt
              } else {
                result -= evalFunction(a, b, inputVTable, inputFTable).toString.toInt
              }
            case UserFunction(a, b) =>
              if(i == 0) {
                result = evalUserFunction(a, b, inputVTable, inputFTable).toString.toInt
              } else {
                result -= evalUserFunction(a, b, inputVTable, inputFTable).toString.toInt
              }
          }
        }
      }
    }
    //println(result)
    result
  }

  def functionMultiply(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Int = {
    var result = 0
    inputArguments match {
      case Argument(group) => {
        for (i <- 0 until group.size) {
          //println(indent + " -'s element(" + i + "): " + group(i))
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
                result = result * number
              }
            case IntNumber(a) =>
              if(i == 0) {
                result = a.toInt
              } else {
                result = result * a.toInt
              }
            case Function(a, b) =>
              if(i == 0) {
                result = evalFunction(a, b, inputVTable, inputFTable).toString.toInt
              } else {
                result = result * evalFunction(a, b, inputVTable, inputFTable).toString.toInt
              }
            case UserFunction(a, b) =>
              if(i == 0) {
                result = evalUserFunction(a, b, inputVTable, inputFTable).toString.toInt
              } else {
                result = result * evalUserFunction(a, b, inputVTable, inputFTable).toString.toInt
              }
          }
        }
      }
    }
    //println(result)
    result
  }
}
