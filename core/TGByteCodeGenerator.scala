import java.io._
import scala.collection.immutable
import scala.collection.mutable.ListBuffer

/**
  * Created by stylejy on 18/11/2015.
  * Thanks GOD for all.
  */
class TGByteCodeGenerator(classname: String) {

  val pw = new PrintWriter(new File(classname+".tgcode"))
  //Using ListBuffer instead of List to make appending each code easier.
  val body = new ListBuffer[String]
  var userFuncBody = new ListBuffer[String]
  var userFunctionTable = Map[String, String]()
  //pretends to be a "locals" size.
  val variables = new Array[String](spaceEvaluator._2)
  //used to generate a code
  var variableTable = Map[String, Int]()
  //Switch to choose where the contents are written, 0(default) in body, 1 in UserFuncBody.
  var switch = 0

  def lineFeed(input: Int): String = {
    val feed = "\n"
    feed * input
  }

  def header: Unit = {
    pw.write(".class public "+classname + lineFeed(1))
    pw.write(".super java/lang/Object" + lineFeed(2))

    pw.write(".method public static main : ([Ljava/lang/String;)V" + lineFeed(1))

    val space = spaceEvaluator
    pw.write(".limit stack " + space._1 + lineFeed(1))
    pw.write(".limit locals " + space._2 + lineFeed(2))

    pw.write("getstatic java/lang/System out Ljava/io/PrintStream;" + lineFeed(2))
  }

  def footer: Unit = {
    pw.write(lineFeed(2))
    pw.write("invokevirtual java/io/PrintStream println (I)V" + lineFeed(2))
    pw.write("return" + lineFeed(1))
    pw.write(".end method" + lineFeed(2))
  }

  def spaceEvaluator: (Int, Int) = {
    (10,10)
  }

  //*********************************** Function Start
  //2 integer numbers addition
  def funcPlus(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
        }
        contents += "iadd" + lineFeed(2)
    }
    bodyWriter(contents)
    contents
  }

  //2 integer numbers substraction
  def funcMinus(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
        }
        contents += "isub" + lineFeed(2)
    }
    bodyWriter(contents)
    contents
  }

  //2 integer numbers multiplication
  def funcMultiply(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
        }
        contents += "imul" + lineFeed(2)
    }
    bodyWriter(contents)
    contents
  }

  //2 integer numbers multiplication
  def funcRemainder(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += "iload " + variableTable(a) + lineFeed(1)
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += "iload " + variableTable(a) + lineFeed(1)
        }
        contents += "irem" + lineFeed(2)
    }
    bodyWriter(contents)
    contents
  }

  def funcEqual(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += "iload " + variableTable(a) + lineFeed(1)
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += "iload " + variableTable(a) + lineFeed(1)
        }
        contents += "if_icmpeq Equal" + lineFeed(1)
        contents += "iconst_0" + lineFeed(1)
        contents += "goto stopequal" + lineFeed(1)
        contents += "Equal:" + lineFeed(1)
        contents += "iconst_1" + lineFeed(1)
        contents += "stopequal:" + lineFeed(2)
    }
    bodyWriter(contents)
    contents
  }

  //Supports multiple Integer arguments
  def funcOr(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        for(i <- group) {
          i match {
            case IntNumber(a) =>
              contents += numberRange(a.toInt)
              contents += "ifgt True" + lineFeed(1)
            case Bool(a) =>
              if (a.equals("true"))
                contents += "iconst_1" + lineFeed(1)
              else
                contents += "iconst_0" + lineFeed(1)
              contents += "ifgt True" + lineFeed(1)
          }
        }
        //If it reaches the next line, it means there's no true statement in the given input.
        //Returns 0
        contents += lineFeed(1) + "iconst_0" + lineFeed(1)
        contents += "goto escapeor" + lineFeed(2)
        //If ifgt finds a true element, the loop stops and returns 1
        contents += "True:" + lineFeed(1)
        contents += "iconst_1" + lineFeed(2)
        //If it fails, it skips the True: part
        contents += "escapeor:" + lineFeed(2)
    }
    bodyWriter(contents)
    contents
  }

  def funcIf(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case Bool("true") => contents += numberRange(1)
          case IntNumber(a) => contents += numberRange(1)
          case Function(a, b) =>
            for(i <- functionSelector(a, b).asInstanceOf[ListBuffer[String]])
              contents += i
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
          case _ => contents += numberRange(0)
        }
        contents += "ifeq False" + lineFeed(1)
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) =>
            for(i <- functionSelector(a, b).asInstanceOf[ListBuffer[String]])
              contents += i
          case Value(a) => contents += "iload " + variableTable(a) + lineFeed(1)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        contents += "goto escapeif" + lineFeed(1)
        contents += "False:" + lineFeed(1)
        group(2) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) =>
            for(i <- functionSelector(a, b).asInstanceOf[ListBuffer[String]])
              contents += i
          case Value(a) => contents += "iload " + variableTable(a) + lineFeed(1)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b)) {
              contents += i
            }
        }
        contents += "escapeif:" + lineFeed(2)
    }
    bodyWriter(contents)
    contents
  }

  def funcDefn(input: Expr): Unit = {
    input match {
      case Argument(group) =>
        if(group.size == 3) {
          if(group.size == 3) {
            val functionName = group(0) match {
              case Value(name) => name
            }
            try {
              userFunctionTable(functionName.toString)
            } catch {
              case ex: NoSuchElementException =>
                switch = 1

                var counter = 1

                val args = group(1) match {
                  case Vector(a) => a
                }
                val numberOfArgs = args.size
                val argGen = "I" * numberOfArgs

                //for function calling from main.
                val functionCall = "invokestatic " + classname + " " + functionName + " (" + argGen + ")I" + lineFeed(1)
                userFunctionTable += (functionName.toString -> functionCall)

                //for generating user function codes.
                val methodTitle = ".method public static " + functionName.toString +" : (" + argGen + ")I" + lineFeed(1)
                userFuncBody += methodTitle
                val space = spaceEvaluator
                userFuncBody += ".limit locals " + space._1 + lineFeed(1)
                userFuncBody += ".limit stack " + space._2 + lineFeed(1)

                for(i <- args) {
                  val name = i match {
                    case Value(a) => a
                  }
                  addLocalVariable(name)

                }

                val bodyContents = evalExpression(group(2)).asInstanceOf[ListBuffer[String]]
                for (i <- bodyContents)
                  userFuncBody += "   " + i

                userFuncBody += "   " + "ireturn" + lineFeed(1)
                userFuncBody += ".end method" + lineFeed(2)

                switch = 0
            }
          }
        }
    }
  }
  //*********************************** Function End

  def evalExpression(expr: Expr): Any = {
    expr match {
      case Value(a) => variableTable(a)
      case Keyword(a) => a
      case IntNumber(a) => a.toInt
      case Bool(a) => a.toBoolean
      case Sentence(a) => a match {
        case Value(a) => a
      }
      case Vector(a) => deriveValueFromVector(a)
      case Argument(a) => deriveValueFromVector(a)
      case Function(a, b) => functionSelector(a, b)
      case UserFunction(a, b) => userFunction(a, b)

    }
  }

  def bodyWriter(input: ListBuffer[String]): Unit = {
    if(switch.equals(0)) {
      for(i <- input)
        body += i
    }
  }

  def userFunction(firstInput: Expr, secondInput: Argument): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    val name = firstInput match {
      case Value(a) => a
    }

    val args = secondInput match {
      case Argument(a) => a
    }
    //val rArgs =  args.reverse
    for (i <- args) {
      val argValue = i match {
        case IntNumber(a) => contents += numberRange(a.toInt)
        case Value(a) => contents +=  "iload " + getLocalVariable(a) + lineFeed(1)
        case Function(a, b) =>
          for(i <- functionSelector(a, b).asInstanceOf[ListBuffer[String]])
            contents += i
      }
    }
    contents += userFunctionTable(name)

    bodyWriter(contents)
    contents
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
    value
  }

  def numberRange(input: Int): String = {
    if(input == -1) {
      "iconst_m1" + lineFeed(1)
    } else if(input <= 5) {
      "iconst_" + input + lineFeed(1)
    } else if(input >= -128 && input <= 127) {
      "bipush " + input + lineFeed(1)
    } else if(input >= -32768 && input <= 32767) {
      "sipush " + input + lineFeed(1)
    } else {
      "ldc " + input + lineFeed(1)
    }
  }

  def addLocalVariable(input: String): Unit = {
    for(i <- variables.indices) {
      try {
        if (variables(i).isEmpty) {
          variables(i) = input
          //also put the information in variableTable.
          variableTable += (input -> i)
          //Has to return something to stop the for loop after adding or wrong result.
          return 0
        } else if(variables(i).equals(input)) {
          return 0
        }
      } catch {
        case ex: NullPointerException =>
          variables(i) = input
          //also put the information in variableTable.
          variableTable += (input -> i)
          //Has to return something to stop the for loop after adding or wrong result.
          return 0
      }
    }
  }

  def getLocalVariable(input: String): Int = {
    variables.indexOf(input)
  }

  def labelManager: Unit = {

  }

  def composer: Unit = {
    header

    for (i <- body) {
      pw.write(i)
    }

    footer

    if(!userFuncBody.isEmpty){
      for (i <- userFuncBody) {
        pw.write(i)
      }
    }
    pw.close()
  }

  def functionSelector(firstInput: Expr, secondInput: Argument): Any = {
    val keyword = firstInput match {
      case Keyword(a) => a.toString
    }

    keyword match {
      case "defn" => funcDefn(secondInput)
      case "if" => funcIf(secondInput)
      case "or" => funcOr(secondInput)
      case "=" => funcEqual(secondInput)
      case "+" => funcPlus(secondInput)
      case "-" => funcMinus(secondInput)
      case "*" => funcMultiply(secondInput)
      case "rem" => funcRemainder(secondInput)
    }
  }

}
