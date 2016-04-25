import scala.Unit
import scala.collection.immutable
import scala.collection.mutable.ListBuffer

/**
  * Created by stylejy on 13/12/2015.
  */
//dependentDepth: Integer number shows depth from the root.
class TGEvaluator {
  //Flag to show if List is used by ListFuctions like nth and first
  var listUsedByListFunction = 0

  def evalExpression(expr: Expr, vTable: TGVariableSymbolTable, fTable: TGFunctionSymbolTable): Any = {
    expr match {
      case Value(a) => evalExpression(vTable.list(a), vTable, fTable)
      case Keyword(a) => a
      case IntNumber(a) => a.toInt
      case Bool(a) => a.toBoolean
      case Vector(a) => deriveValueFromVector(a)
      case Argument(a) => deriveValueFromVector(a)
      case Function(a, b) => evalFunction(a, b, vTable, fTable)
      case UserFunction(a, b) => evalUserFunction(a, b, vTable, fTable)
    }
  }

  def evalUserFunction(firstInput: Expr, secondInput: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Any ={
    //println(indent + "[CALL] User function: " + firstInput)
    val userInputArg = secondInput match {
      case Argument(a) => a
    }
    val arg = new Array[Expr](userInputArg.size)
    for (i <- userInputArg.indices) {
      if(userInputArg(i) match {
        case Function(a, b) => true
        case UserFunction(a, b) => true
        case Value(a) => true
        case _ => false
      }) {
        arg(i) = userInputArg(i) match {
          case Value(a) => IntNumber(evalExpression(inputVTable.list(a), inputVTable, inputFTable).toString)
          case Function(a, b) => IntNumber(evalFunction(a, b, inputVTable, inputFTable).toString)
          case UserFunction(a, b) => IntNumber(evalUserFunction(a, b, inputVTable, inputFTable).toString)
        }
      } else {
        arg(i) = userInputArg(i)
      }
      //println(userInputArg(i) + "---->" + arg(i))
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

    if(arg.length.equals(vars.length)) {
      var counter = 0
      for(i <- vars) {
        val varName = i match {
          case Value(a) => a.toString
        }
        newVTable.list.update(varName, arg(counter))
        //println(varName + " links to " + arg(counter))
        counter += 1
      }
    }
    evalExpression(inputFTable.list(name)._2, newVTable, inputFTable)
  }

  def evalFunction(firstInput: Expr, secondInput: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable ): Any = {
    val keyword = evalExpression(firstInput, inputVTable, inputFTable).toString
    keyword match {
      case "defn" => functionDefn(secondInput, inputVTable, inputFTable)
      case "if" =>
        functionIf(secondInput, inputVTable, inputFTable)
      case "or" =>
        functionOr(secondInput, inputVTable, inputFTable)
      case "=" =>
        functionEqual(secondInput, inputVTable, inputFTable)
      case "+" =>
        functionPlus(secondInput, inputVTable, inputFTable)
      case "-" =>
        functionMinus(secondInput, inputVTable, inputFTable)
      case "*" =>
        functionMultiply(secondInput, inputVTable, inputFTable)
      case "rem" =>
        functionRemainder(secondInput, inputVTable, inputFTable)
      case "println" =>
        functionPrintln(secondInput, inputVTable, inputFTable)
      case "list" =>
        functionList(secondInput, inputVTable, inputFTable)
      case "nth" =>
        functionListNth(secondInput, inputVTable, inputFTable)
      case "rest" =>
        functionRest(secondInput, inputVTable, inputFTable)
      case "sort" =>
        functionSort(secondInput, inputVTable, inputFTable)
    }
  }

  def deriveValueFromVector(inputVector: immutable.Seq[Expr]): String = {
    var value = ""
    for (i <- inputVector) {
      value = i match {
        case Value(name) => name.toString
      }
    }
    value
  }

  def functionRest(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Any = {
    listUsedByListFunction = 1

    val buffer = inputArguments match {
      case Argument(group) =>
        val list = evalExpression(group.head, inputVTable, inputFTable).asInstanceOf[ListBuffer[Any]]
        list.remove(0)
        list
    }

    listUsedByListFunction = 0
    buffer
  }

  //*************************Sort needs changing.
  def functionSort(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Any = {
    listUsedByListFunction = 1

    val buffer = inputArguments match {
      case Argument(group) =>
        val list = evalExpression(group.head, inputVTable, inputFTable).asInstanceOf[ListBuffer[Any]]
        var finishSign = 0

        while (finishSign.equals(0)) {
          var swapSign = 0
          for (i <- 0 to list.size - 2) {
            if (list(i).asInstanceOf[Int] > list(i + 1).asInstanceOf[Int]) {
              val temp = list(i)
              list.update(i, list(i + 1))
                list.update(i + 1, temp)
                swapSign = 1 //If at least once swapping happens swapSign will be 1
              }
            }
          if(swapSign.equals(0)) //swapSign 0 meas any value of the list was swapped. In other words, all values are in the right order.
            finishSign = 1
        }

        list
    }

    listUsedByListFunction = 0
    buffer
  }


  def functionListNth(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Any = {
    var result = ""
    var nested = 0

    if(listUsedByListFunction.equals(1))
      nested = 1

    listUsedByListFunction = 1
    inputArguments match {
      case Argument(group) =>
        val list = evalExpression(group.head, inputVTable, inputFTable).asInstanceOf[ListBuffer[Any]]
        val number = group(1) match {
          case IntNumber(a) => a.toInt
        }
        val extractedValue = list(number)

        if(extractedValue.isInstanceOf[ListBuffer[Any]]) {
          if (nested.equals(0))
            result = extractedValue.asInstanceOf[ListBuffer[Any]].mkString("(", ", ", ")")
          else
          //for (nth (nth (list 1 (list 1 2)) 1) 1)
            return extractedValue.asInstanceOf[ListBuffer[Any]]
        }
        else
          result = extractedValue.toString()
    }
    listUsedByListFunction = 0
    result
  }

  def functionList(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Any = {
    val buffer = new ListBuffer[Any]
    inputArguments match {
      case Argument(group) =>
        for (i <- 0 until group.size) {
          buffer += evalExpression(group(i), inputVTable, inputFTable)
        }
    }
    if (listUsedByListFunction.equals(0))
      buffer.mkString("(", ", ", ")")
    else
      buffer
  }

  def functionPrintln(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Any = {
    inputArguments match {
      case Argument(arg) => evalExpression(arg(0), inputVTable, inputFTable)
    }
  }

  def functionDefn(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Unit = {
    inputArguments match {
      case Argument(group) => {
        if(group.size == 3) {
          /*
          val parameter = group(1) match {
            case Vector(name) => deriveValueFromVector(name)
          }
          */
          //println(indent + " User declared function name: " + group(0))
          val functionName = group(0) match {
            case Value(name) => name.toString
          }
          try {
            inputFTable.list(functionName.toString)
          } catch {
            case ex: NoSuchElementException =>
              inputFTable.list += (functionName.toString -> (group(1),group(2)))
/*
              try {
                inputVTable.list(parameter.toString)
              } catch {
                case ex: NoSuchElementException =>
                  inputVTable.list += (parameter.toString -> Empty(0))
              }
              */
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

          if (evalExpression(group(0), inputVTable, inputFTable).toString.toBoolean) {
            evalExpression(group(1), inputVTable, inputFTable)
          }
          else {
            evalExpression(group(2), inputVTable, inputFTable)
          }
        }
      }
    }
  }

  def functionOr(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Boolean = {
    var result = false
    inputArguments match {
      case Argument(group) =>
        for (i <- 0 until group.size)
          //println(indent + " or's condition(" + i + "): " + group(i))
          group(i) match {
            case Value(a) =>
              if(inputVTable.list(a).equals("true")) {
                //println(true)
                result = true
              }
            case IntNumber(a) =>
              //println(true)
              result = true
            case Bool(a) =>
              if(a.toBoolean) {
                //println(true)
                result = true
              }
            case Function(a, b) =>
              if(evalFunction(a, b, inputVTable, inputFTable).equals(true)) {
                //println(true)
                result = true
              }
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
    }
    //println(result)
    result
  }

  //Accept only two arguments
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
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
          val value2 = group(1) match {
            case Value(a) => evalExpression(inputVTable.list(a), inputVTable, inputFTable)
            case IntNumber(a) => a.toInt
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

  def functionRemainder(inputArguments: Argument, inputVTable: TGVariableSymbolTable, inputFTable: TGFunctionSymbolTable): Int = {
    var result = 0
    inputArguments match {
      case Argument(group) => {
        if(group.size == 2) {
          //println(indent + " ='s first element: " + group(0))
          //println(indent + " ='s second element: " + group(1))
          val value1 = group(0) match {
            case Value(a) =>
              //println("a: "+a)
              //println("table: " +inputVTable.list(a))
              inputVTable.list(a) match {
              case IntNumber(a) => a.toInt
            }
            case IntNumber(a) =>
              a.toInt
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
          val value2 = group(1) match {
            case Value(a) => inputVTable.list(a) match {
              case IntNumber(a) => a.toInt
            }
            case IntNumber(a) => a.toInt
            case Function(a, b) => evalFunction(a, b, inputVTable, inputFTable)
            case UserFunction(a, b) => evalUserFunction(a, b, inputVTable, inputFTable)
          }
          result = value1.toString.toInt % value2.toString.toInt
        }
      }
    }
    //println(result)
    result
  }
}
