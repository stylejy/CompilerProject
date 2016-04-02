import java.io._
import scala.collection.{mutable, immutable}
import scala.collection.mutable.ListBuffer

/**
  * Created by stylejy on 18/11/2015.
  * Thanks GOD for all.
  */
class TGByteCodeGenerator(classname: String) {
  //whole ast initially empty
  var ast = ""
  val pw = new PrintWriter(new File(classname+".tgcode"))
  //Using ListBuffer instead of List to make appending each code easier.
  val body = new ListBuffer[String]
  var userFuncBody = new ListBuffer[String]
  var userFunctionTable = Map[String, String]()
  //variableTable for main
  var variableTable = Map[String, Int]()
  //variableTable for userFunction
  var userFunctionVariableTable = Map[String, mutable.Map[String, Int]]()
  //Switch to choose where the contents are written, 0(default) in body, 1 in UserFuncBody.
  //Switch also let a function knows who's called.
  //switch._1 shows if userFunction works and switch._2 shows the userFunction's name.
  var userFunctionSwitch = (0, "")
  //Indicates if a calculation function like arithmetic functions and boolean functions is processed or not
  var calculationFunctionSwitch = (0, "")
  var functionSwitch = (0, "")
  //Used to avoid using the same labels are written many times.
  var labelTable = Map[String, Int]()
  //Shows how many recursive calls calling itself are used inside the UserFunction code
  var numberOfRecursiveCall = 0
  val spaceEstimator = new TGSpaceEstimator
  var spaceForUserFunc = Map[String, (Int, Int)]()
  //Sizes of local and stack should be at least 1 each.
  var spaceForMain = (1, 1)
  //Println output parameter value, default is I (which means interger value).
  var outParam = "I"


  def lineFeed(input: Int): String = {
    val feed = "\n"
    feed * input
  }

  def header: Unit = {
    pw.write(".class public "+classname + lineFeed(1))
    pw.write(".super java/lang/Object" + lineFeed(2))

    pw.write(".method public static main : ([Ljava/lang/String;)V" + lineFeed(1))

    pw.write(".limit locals " + spaceForMain._1 + lineFeed(1))
    pw.write(".limit stack " + spaceForMain._2 + lineFeed(2))
  }

  def footer: Unit = {
    pw.write(lineFeed(2))
    pw.write("return" + lineFeed(1))
    pw.write(".end method" + lineFeed(2))
  }

  //*********************************** Function Start
  //2 integer numbers addition
  def funcPlus(input: Expr): ListBuffer[String] = {
    val name = "plus"
    if(calculationFunctionSwitch._1.equals(0))
      calculationFunctionSwitch = (1, name)
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += singleInt(a)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        contents += "iadd" + lineFeed(2)
    }
    if(calculationFunctionSwitch._2.equals(name))
      calculationFunctionSwitch = (0, "")
    bodyWriter(contents)
    contents
  }

  //2 integer numbers substraction
  def funcMinus(input: Expr): ListBuffer[String] = {
    val name = "minus"
    if(calculationFunctionSwitch._1.equals(0))
      calculationFunctionSwitch = (1, name)
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        contents += "isub" + lineFeed(2)
    }
    if(calculationFunctionSwitch._2.equals(name))
      calculationFunctionSwitch = (0, "")
    bodyWriter(contents)
    contents
  }

  //2 integer numbers multiplication
  def funcMultiply(input: Expr): ListBuffer[String] = {
    val name = "multi"
    if(calculationFunctionSwitch._1.equals(0))
      calculationFunctionSwitch = (1, name)
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for (i <- userFunction(a, b))
              contents += i
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for (i <- userFunction(a, b))
              contents += i
        }
        contents += "imul" + lineFeed(2)
    }
    if(calculationFunctionSwitch._2.equals(name))
      calculationFunctionSwitch = (0, "")
    bodyWriter(contents)
    contents
  }

  //2 integer numbers remainder
  def funcRemainder(input: Expr): ListBuffer[String] = {
    val name = "rem"
    if(calculationFunctionSwitch._1.equals(0))
      calculationFunctionSwitch = (1, name)
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        contents += "irem" + lineFeed(2)
    }
    if(calculationFunctionSwitch._2.equals(name))
      calculationFunctionSwitch = (0, "")
    bodyWriter(contents)
    contents
  }

  def funcEqual(input: Expr): ListBuffer[String] = {
    val name = "equal"
    if(calculationFunctionSwitch._1.equals(0))
      calculationFunctionSwitch = (1, name)
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        group(0) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) => functionSelector(a, b)
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        val labelEqual = labelManager("EQUAL")
        val labelStopEqual = labelManager("STOPEQUAL")
        contents += "if_icmpeq " + labelEqual + lineFeed(1)
        contents += "iconst_0" + lineFeed(1)
        contents += "goto " + labelStopEqual + lineFeed(1)
        contents +=  labelEqual + ":" + lineFeed(1)
        contents += "iconst_1" + lineFeed(1)
        contents += labelStopEqual + ":" + lineFeed(2)
    }
    if(calculationFunctionSwitch._2.equals(name))
      calculationFunctionSwitch = (0, "")
    bodyWriter(contents)
    contents
  }

  //Supports multiple Integer arguments
  def funcOr(input: Expr): ListBuffer[String] = {
    val name = "or"
    if(calculationFunctionSwitch._1.equals(0))
      calculationFunctionSwitch = (1, name)
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        val labelTrue = labelManager("TRUE")
        val labelEscapeor = labelManager("ESCAPEOR")
        for(i <- group) {
          i match {
            case Value(a) => contents += singleValue(a)
            case IntNumber(a) =>
              contents += numberRange(a.toInt)
              contents += "ifgt " + labelTrue + lineFeed(1)
            case Bool(a) =>
              if (a.equals("true"))
                contents += "iconst_1" + lineFeed(1)
              else
                contents += "iconst_0" + lineFeed(1)
              contents += "ifgt " + labelTrue + lineFeed(1)
            case UserFunction(a, b) =>
              for(i <- userFunction(a, b))
                contents += i
          }
        }
        //If it reaches the next line, it means there's no true statement in the given input.
        //Returns 0
        contents += lineFeed(1) + "iconst_0" + lineFeed(1)
        contents += "goto " + labelEscapeor + lineFeed(2)
        //If ifgt finds a true element, the loop stops and returns 1
        contents += labelTrue + ":" + lineFeed(1)
        contents += "iconst_1" + lineFeed(2)
        //If it fails, it skips the True: part
        contents += labelEscapeor + ":" + lineFeed(2)
    }
    if(calculationFunctionSwitch._2.equals(name))
      calculationFunctionSwitch = (0, "")
    bodyWriter(contents)
    contents
  }

  def funcIf(input: Expr): ListBuffer[String] = {
    val name = "if"
    if(functionSwitch._1.equals(0))
      functionSwitch = (1, name)
    val contents = new ListBuffer[String]
    input match {
      case Argument(group) =>
        val labelFalse = labelManager("FALSE")
        val labelEscapeif = labelManager("ESCAPEIF")
        group(0) match {
          case Value(a) => contents += singleValue(a)
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
        contents += "ifeq " + labelFalse + lineFeed(1)
        group(1) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) =>
            for(i <- functionSelector(a, b).asInstanceOf[ListBuffer[String]])
              contents += i
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b))
              contents += i
        }
        contents += "goto " + labelEscapeif + lineFeed(1)
        contents += labelFalse + ":" + lineFeed(1)
        group(2) match {
          case IntNumber(a) => contents += numberRange(a.toInt)
          case Function(a, b) =>
            for(i <- functionSelector(a, b).asInstanceOf[ListBuffer[String]])
              contents += i
          case Value(a) => contents += singleValue(a)
          case UserFunction(a, b) =>
            for(i <- userFunction(a, b)) {
              contents += i
            }
        }
        contents += labelEscapeif + ":" + lineFeed(2)
    }
    if(functionSwitch._2.equals(name))
      functionSwitch = (0, "")
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
                //switch 1 means defn function is processed.
                userFunctionSwitch = (1, functionName)

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
                val space = spaceForUserFunc(functionName)
                userFuncBody += ".limit locals " + space._1 + lineFeed(1)
                userFuncBody += ".limit stack " + space._2 + lineFeed(1)

                //Starting label avoid recursive functiona call
                userFuncBody += functionName.toUpperCase + "START:" + lineFeed(1)

                for(i <- args) {
                  val varName = i match {
                    case Value(a) => a
                  }
                  addLocalVariable(functionName, varName)

                }

                val bodyContents = evalExpression(group(2)).asInstanceOf[ListBuffer[String]]
                for (i <- bodyContents)
                  userFuncBody += "   " + i

                userFuncBody += "   " + "ireturn" + lineFeed(1)
                userFuncBody += ".end method" + lineFeed(2)

                //switch 2 means defn function is done.
                userFunctionSwitch = (0, "")
            }
          }
        }
    }
  }

  //It saves its parent's functionSwitch information and put the information back to the switch.
  def funcList(input: Expr): ListBuffer[String] = {
    val name = "list"
    val contents = new ListBuffer[String]
    var nested = 0
    var parentName = ""
    outParam = "Ljava/lang/Object;"

    if(functionSwitch._1.equals(0))
      functionSwitch = (1, name)
    else {
      contents += lineFeed(1)
      nested = 1
      parentName = functionSwitch._2
      functionSwitch = (1, name)
    }

    contents += "new java/util/ArrayList" + lineFeed(1)
    contents += "dup" + lineFeed(1)
    contents += "invokespecial java/util/ArrayList <init> ()V" + lineFeed(1)
    val numberOfArgs = variableTable.size
    var variableNumber = 0
    if (!numberOfArgs.equals(0))
      variableNumber = numberOfArgs
    contents += "astore " + variableNumber + lineFeed(1)
    val info = ("list" + variableNumber -> variableNumber)
    variableTable += info

    input match {
      case Argument(group) =>
        for(i <- group) {
          contents += "aload " + variableNumber + lineFeed(1)
          val evaluatedValue = evalExpression(i)
          //Check if the list is nested in another list.
          if(evaluatedValue.isInstanceOf[ListBuffer[String]]) {
            for(j <- evaluatedValue.asInstanceOf[ListBuffer[String]])
              contents += j
          }
          else {
            contents += evaluatedValue.toString
          }
          contents += "invokevirtual java/util/ArrayList add (Ljava/lang/Object;)Z" + lineFeed(1)
          contents += "pop" + lineFeed(1)
        }
        if(nested.equals(1))
          contents += "aload " + variableNumber + lineFeed(1)
    }

    contents += lineFeed(2)

    //Put 'I' back to outParam.
    //outParam = "I"

    if(nested.equals(0)) {
      bodyWriter(contents)
    }
    if(functionSwitch._2.equals(name))
      functionSwitch = (1, parentName)
    else
      functionSwitch = (0, "")
    contents
  }

  //It saves its parent's functionSwitch information and put the information back to the switch.
  def funcListNth(input: Expr): ListBuffer[String] = {
    val name = "nth"
    val contents = new ListBuffer[String]
    var nested = 0
    var parentName = ""
    outParam = "Ljava/lang/Object;"

    if(functionSwitch._1.equals(0))
      functionSwitch = (1, name)
    else {
      nested = 1
      parentName = functionSwitch._2
      functionSwitch = (1, name)
    }

    input match {
      case Argument(group) =>
        //Only accepts list function for the first argument
        group(0) match {
          case Function(a, b) => a match {
            case Keyword(a) =>
              if(a.equals("list"))
                for(i <- evalExpression(group(0)).asInstanceOf[ListBuffer[String]])
                  contents += i
          }
        }

        group(1) match {
          case IntNumber(a) =>
            contents += evalExpression(group(1)).toString
        }

        contents += "invokevirtual java/util/ArrayList get (I)Ljava/lang/Object;" + lineFeed(1)

    }
    if(functionSwitch._2.equals(name))
      functionSwitch = (1, parentName)
    else
      functionSwitch = (0, "")
    bodyWriter(contents)
    contents
  }
  //*********************************** Function End

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
      case "println" => funcPrintln(secondInput)
      case "list" => funcList(secondInput)
      case "nth" => funcListNth(secondInput)
    }
  }

  //*********************************** Single Value start
  def singleInt(input: String): String = {
    val contentsForBodyWriter = new ListBuffer[String]
    var contentsForReturn = ""

    val numResult = numberRange(input.toInt)
    contentsForBodyWriter += numResult
    contentsForReturn = numResult

    if(functionSwitch._1.equals(0) & userFunctionSwitch._1.equals(0) & calculationFunctionSwitch._1.equals(0)) {
      bodyWriter(contentsForBodyWriter)
    }
    contentsForReturn
  }

  def singleValue(input: String): String = {
    val contentsForBodyWriter = new ListBuffer[String]
    var contentsForReturn = ""
    var numResult = ""

    if(userFunctionSwitch._1.equals(0)) {
      numResult = "iload " + variableTable(input) + lineFeed(1)
    } else {
      numResult = "iload " + userFunctionVariableTable(userFunctionSwitch._2)(input) + lineFeed(1)
    }

    contentsForBodyWriter += numResult
    contentsForReturn = numResult

    bodyWriter(contentsForBodyWriter)
    contentsForReturn
  }
  //*********************************** Single Value End

  def run(expr: Expr): Unit = {
    ast = expr.toString
    val spaceResult = spaceEstimator.run(expr)
    expr match {
      case Function(Keyword("defn"), b) =>
        b match {
          case Argument(group) =>
            val funcName = group(0) match {
              case Value(a) => a
            }
            //For the given line which is the function calling defn.
            spaceForUserFunc += (funcName -> spaceResult)
        }
      case _ =>
        //For functions except defn to decide the main's space.
        if(spaceForMain._1 < spaceResult._1)
          spaceForMain = (spaceResult._1, spaceForMain._2)
        if(spaceForMain._2 < spaceResult._2)
          spaceForMain = (spaceForMain._1, spaceResult._2)
    }
    evalExpression(expr)
  }

  def evalExpression(expr: Expr): Any = {
    expr match {
      case Value(a) => singleValue(a)
      case Keyword(a) => a
      case IntNumber(a) => singleInt(a)
      case Bool(a) => a.toBoolean
      case Sentence(a) => a match {
        case Value(a) => a
      }
      case Vector(a) => deriveValueFromVector(a)
      case Argument(a) => deriveValueFromVector(a)
      case Function(a, b) => functionSelector(a, b)
      case UserFunction(a, b) => userFunction(a, b)
      //case ListQuote(a) =>
    }
  }

  def bodyWriter(input: ListBuffer[String]): Unit = {
    if(userFunctionSwitch._1.equals(0)) {
      for(i <- input)
        body += "   " + i
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

    countRecursiveCalls(name)

    for (i <- args) {
      i match {
        case IntNumber(a) => contents += numberRange(a.toInt)
        case Value(a) =>
          //If user function is called in funcDefn, variable is looked up with the function name used in funcDefn.
          //Otherwise, it uses userFunction's name.
          if(userFunctionSwitch._1.equals(0))
            contents +=  "iload " + getLocalVariable(name, a) + lineFeed(1)
          else
            contents +=  "iload " + getLocalVariable(userFunctionSwitch._2, a) + lineFeed(1)
        case Function(a, b) =>
          for(i <- functionSelector(a, b).asInstanceOf[ListBuffer[String]])
            contents += i
        case UserFunction(a, b) =>
          for(i <- userFunction(a, b)) {
            contents += i
          }
      }
    }

    //Efficient code only works with only one recursive call and it shouldn't be with calculation functions.
    if(name.equals(userFunctionSwitch._2) && numberOfRecursiveCall.equals(1) && calculationFunctionSwitch.equals(0)) {
      var numberOfArgs = args.size
      while (numberOfArgs > 0) {
        contents += "istore " + (numberOfArgs - 1) + lineFeed(1)
        numberOfArgs = numberOfArgs - 1
      }
      contents += "goto " + name.toUpperCase + "START" + lineFeed(1)
    } else {
      //userFunctionTable returns invokestatic code for the given parameter name.
      contents += userFunctionTable(name)
    }

    bodyWriter(contents)
    contents
  }

  def countRecursiveCalls(input: String): Unit = {
    val array = ast.split(input)
    //array.size says the larger number by 2 than the number of its actual recursive calls
    numberOfRecursiveCall = array.size - 2
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
    if(functionSwitch._2.equals("list"))
      "ldc " + "'" + input + "'" + lineFeed(1)
    else {
      if (input == -1) {
        "iconst_m1" + lineFeed(1)
      } else if (input <= 5) {
        "iconst_" + input + lineFeed(1)
      } else if (input >= -128 && input <= 127) {
        "bipush " + input + lineFeed(1)
      } else if (input >= -32768 && input <= 32767) {
        "sipush " + input + lineFeed(1)
      } else {
        "ldc " + input + lineFeed(1)
      }
    }
  }

  def addLocalVariable(firstInput: String, secondInput: String): Unit = {
    val functionName = firstInput
    val variableName = secondInput

    println("add Local Variable: " + functionName + " " + variableName)

    if(userFunctionSwitch._1.equals(1)) {
      try {
        val numberOfArgs = userFunctionVariableTable(functionName).size
        val info = (variableName -> numberOfArgs)
        userFunctionVariableTable(functionName) += info
      } catch {
        case ex: NoSuchElementException =>
          var info = mutable.Map[String, Int]()
          info += (variableName -> 0)
          userFunctionVariableTable += (functionName -> info)
      }
    } else {
      val numberOfArgs = variableTable.size
      if (!numberOfArgs.equals(0)) {
        val info = (variableName -> numberOfArgs)
        variableTable += info
      }
      else {
        val info = (variableName -> 0)
        variableTable += info
      }
    }
  }

  def getLocalVariable(firstInput: String, secondInput: String): Int = {
    val functionName = firstInput
    val variableName = secondInput
    //refurns the local variable number by function
    if(!functionName.equals("main")) {
      println("get Local Variable: " + functionName + " " + variableName)
      userFunctionVariableTable(functionName)(variableName)
    }
    else
      variableTable(variableName)
  }

  def labelManager(input: String): String = {
    if(labelTable.contains(input)) {
      //If a function tries to use the same label already existed, labelTable gives the incremented number to differentiate.
      val incrementedNumber = labelTable(input)+1
      labelTable += (input -> incrementedNumber)
      input + labelTable(input)
    } else {
      labelTable += (input -> 0)
      input + 0
    }
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

  def funcPrintln(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]

    //Main parts will be added first than the getstatic below, therefore bodyWriter should come just after the getstatic line.
    contents += "getstatic java/lang/System out Ljava/io/PrintStream;" + lineFeed(2)
    bodyWriter(contents)

    input match {
      case Argument(arg) => evalExpression(arg.head)
    }

    //Before going to bodyWriter again, getstatic should be removed first or it gets duplication.
    contents -= "getstatic java/lang/System out Ljava/io/PrintStream;" + lineFeed(2)
    contents += "invokevirtual java/io/PrintStream println (" + outParam + ")V" + lineFeed(2)
    bodyWriter(contents)

    contents
  }
}