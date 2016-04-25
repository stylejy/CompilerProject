import java.io._

import scala.collection.mutable.ListBuffer
import scala.collection.{GenSeq, immutable, mutable}

/**
  * Created by stylejy on 18/11/2015.
  * Thanks GOD for all.
  */
class TGCodeGenerator(classname: String) {
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
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }
    functionSwitch = (1, "plus")

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
    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    contents
  }

  //2 integer numbers substraction
  def funcMinus(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }
    functionSwitch = (1, "minus")

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
    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    contents
  }

  //2 integer numbers multiplication
  def funcMultiply(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }
    functionSwitch = (1, "multi")

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
    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    contents
  }

  //2 integer numbers remainder
  def funcRemainder(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }
    functionSwitch = (1, "rem")

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
    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    contents
  }

  def funcEqual(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }
    functionSwitch = (1, "equal")

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

    }

    val hEqual = helperComparison("equal")
    for(i <- hEqual)
      contents += i

    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    contents
  }

  //Supports multiple Integer arguments
  def funcOr(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }
    functionSwitch = (1, "or")

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

    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    contents
  }

  def funcIf(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }

    functionSwitch = (1, "if")

    input match {
      case Argument(group) =>
        val hIf = helperIf("equal")

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

        contents += hIf._1

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

        contents += hIf._2
        contents += hIf._3

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
        contents += hIf._4
    }
    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    contents
  }

  def funcDefn(input: Expr): Unit = {
    val previousSwitch = functionSwitch
    functionSwitch = (1, "defn")

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
                val arguments = "I" * numberOfArgs

                //for function calling from main.
                val functionCall = "invokestatic " + classname + " " + functionName + " (" + arguments + ")I" + lineFeed(1)
                userFunctionTable += (functionName.toString -> functionCall)

                //for generating user function codes.
                val methodTitle = ".method public static " + functionName.toString +" : (" + arguments + ")I" + lineFeed(1)
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

                val bodyContents = evalExpression(group(2))
                if (bodyContents.isInstanceOf[ListBuffer[String]]) {
                  for (i <- bodyContents.asInstanceOf[ListBuffer[String]])
                    userFuncBody += "   " + i
                }
                else
                  userFuncBody += "   " + bodyContents.toString

                userFuncBody += "   " + "ireturn" + lineFeed(1)
                userFuncBody += ".end method" + lineFeed(2)

                //switch 2 means defn function is done.
                userFunctionSwitch = (0, "")
            }
          }
        }
    }
    functionSwitch = previousSwitch
  }

  //It saves its parent's functionSwitch information and put the information back to the switch.
  def funcList(input: Expr): (ListBuffer[String], Int) = {
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    outParam = "Ljava/lang/Object;"

    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }

    functionSwitch = (1, "list")

    //New list
    val hNewList = helperNewList
    for(i <- hNewList._1)
      contents += i
    val variableNumber = hNewList._2

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
          contents += "invokestatic java/lang/Integer valueOf (I)Ljava/lang/Integer;" + lineFeed(1) //For converting primitive integer to object Integer.
          contents += "invokevirtual java/util/ArrayList add (Ljava/lang/Object;)Z" + lineFeed(1)
          contents += "pop" + lineFeed(1)
        }
        //println(functionSwitch._2)
        if((nested.equals(1) || previousSwitch._2.equals("println")) && !previousSwitch._2.equals("rest"))
          contents += "aload " + variableNumber + lineFeed(1)
    }

    contents += lineFeed(2)

    //Put 'I' back to outParam.
    //outParam = "I"

    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    (contents, variableNumber)
  }

  //It saves its parent's functionSwitch information and put the information back to the switch.
  def funcListNth(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    outParam = "Ljava/lang/Object;"

    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }

    functionSwitch = (1, "nth")

    input match {
      case Argument(group) =>
        //Only accepts list function for the first argument
        group(0) match {
          case Function(a, b) => a match {
            case Keyword(a) =>
              if(a.equals("list") || a.equals("rest"))
                for(i <- evalExpression(group(0)).asInstanceOf[(ListBuffer[String], Int)]._1)
                  contents += i
          }
        }

        group(1) match {
          case IntNumber(a) =>
            contents += evalExpression(group(1)).toString
        }

        contents += helperNth()

    }
    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    contents
  }

  def funcRest(input: Expr): (ListBuffer[String], Int, String) = {
    val contents = new ListBuffer[String]
    var nested = 0
    val previousSwitch = functionSwitch
    outParam = "Ljava/lang/Object;"

    if (functionSwitch._1.equals(1)) {
      nested = 1
    }

    functionSwitch = (1, "rest")

    var referenceNumberOfList = -1

    input match {
      case Argument(group) =>
        val value = evalExpression(group.head)
        if(value.isInstanceOf[ListBuffer[String]]) {
          for (i <- value.asInstanceOf[ListBuffer[String]]) {
            contents += i
          }
        }
        else {
          for (i <- value.asInstanceOf[(ListBuffer[String], Int)]._1) {
            contents += i
          }
          referenceNumberOfList = value.asInstanceOf[(ListBuffer[String], Int)]._2.toInt
        }
    }

    //For only sort function
    val labelForSort = {
      val labelResult = labelManager("OUTER")
      if (previousSwitch._2.equals("sort")) {
        contents += labelResult + ":" + lineFeed(1)
      }
      labelResult
    }

    contents += "aload " + referenceNumberOfList + lineFeed(1)
    contents += "iconst_0" + lineFeed(1)
    contents += "invokevirtual java/util/ArrayList remove (I)Ljava/lang/Object;" + lineFeed(1)

    functionSwitch = previousSwitch

    if (nested.equals(0))
      bodyWriter(contents)
    functionSwitch._2 match {
      //invokevirtual remove always returns dropped value so I should be popped out of the stack if it is in another method except println and sort.
      case "sort" | "println" =>
      case _ => contents += "pop" + lineFeed(1)
    }
    functionSwitch._2 match {
      case "rest" | "nth" => contents += "aload " + referenceNumberOfList + lineFeed(1)
      case _ =>
    }
    (contents, referenceNumberOfList, labelForSort)
  }

  def funcSort(input: Expr): ListBuffer[String] = {
    val contents = new ListBuffer[String]
    val previousSwitch = functionSwitch
    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }
    functionSwitch = (1, "sort")
    outParam = "Ljava/lang/Object;"


    //1. New list.
    val hNewList = helperNewList
    for(i <- hNewList._1)
      contents += i
    val newListVariableNumber = hNewList._2

    //2. Send the given list to Rest.
    //Rest will generate outer label for this function
    var labelOuter = ""
    val inputListReferenceNumber = {
      input match {
        case Argument(group) =>
          val tailList = funcRest(Argument(group))
          for (i <- tailList._1.asInstanceOf[ListBuffer[String]])
            contents += i
          //get outerlabel
          labelOuter = tailList._3
          tailList._2
      }
    }

    //2.1 Cast the type(Object -> int) and the store the head as a local variable
    //** can't use dup here because invokevirtual is used later.
    contents += "checkcast java/lang/Integer" + lineFeed(1)
    contents += "invokevirtual java/lang/Integer intValue ()I" + lineFeed(1)
    val hStoreHead = helperStore("i", "sort")
    contents += hStoreHead._1 + lineFeed(1)

    //3. Get given list's size and store the size into the local space.
    contents += "aload " + inputListReferenceNumber + lineFeed(1)
    contents += "invokevirtual java/util/ArrayList size ()I" + lineFeed(1)
    contents += "dup" + lineFeed(1)
    val hStore = helperStore("i", "sort")
    contents += hStore._1 + lineFeed(1)
    val nthCounterReferenceNumber = hStore._2

    //4.1 Check if size is not zero.
    val hIfCheckzero = helperComparison("equalzero")
    for(i <- hIfCheckzero)
      contents += i

    //4.2 Unless size is not zero, start comparing.
    val hIf1 = helperIf("notequal")
    contents += hIf1._1
    //nth number - 1 because nth number is size which is always 1 greater than what we expected to call from the list.
    contents += "iinc " + nthCounterReferenceNumber + " -1" + lineFeed(1)

    //4.3 Put a label for the inner loop
    val labelInner = labelManager("INNER")
    contents += labelInner + ":" + lineFeed(1)

    //**catch if nthCounterNumber is less than zero
    //it happens once the iteration is done and the head should be the smallest value except already in the new list.
    contents += "iload " + nthCounterReferenceNumber + lineFeed(1)
    val hIfCatchLessZero = helperIf("lesszero")
    contents += hIfCatchLessZero._1 + lineFeed(1)
    contents += hIfCatchLessZero._2
    contents += hIfCatchLessZero._3

    contents += "aload " + newListVariableNumber + lineFeed(1)
    contents += "iload " + hStoreHead._2 + lineFeed(1)
    contents += "invokestatic java/lang/Integer valueOf (I)Ljava/lang/Integer;" + lineFeed(1)
    contents += "invokevirtual java/util/ArrayList add (Ljava/lang/Object;)Z" + lineFeed(1)
    contents += "pop" + lineFeed(1)
    contents += "goto " + labelOuter + lineFeed(1)

    contents += hIfCatchLessZero._4 + lineFeed(1)

    //4.4 if outer false(not zero)
    contents += "aload " + inputListReferenceNumber + lineFeed(1)
    contents += "iload " + nthCounterReferenceNumber + lineFeed(1)
    //get a value and cast it to int
    contents += helperNth()
    contents += "checkcast java/lang/Integer" + lineFeed(1)
    contents += "invokevirtual java/lang/Integer intValue ()I" + lineFeed(1)

    //4.5 Call the head and then comparison of the list's head with a element of the tail.
    contents += "iload " + hStoreHead._2 + lineFeed(1)
    val hGreater = helperComparison("lesser")
    for(i <- hGreater)
      contents += i

    //4.6 Inner loop start
    val hIf2 = helperIf("equal")
    contents += hIf2._1

    //if inner false
    contents += "aload " + inputListReferenceNumber + lineFeed(1)
    contents += "iload " + nthCounterReferenceNumber + lineFeed(1)
    contents += helperNth()

    //Store the value to swap
    val hStoreTemp = helperStore("a", "sort")
    contents += hStoreTemp._1 + lineFeed(1)
    contents += "aload " + inputListReferenceNumber + lineFeed(1)
    contents += "iload " + nthCounterReferenceNumber + lineFeed(1)
    contents += "iload " + hStoreHead._2 + lineFeed(1)
    contents += "invokestatic java/lang/Integer valueOf (I)Ljava/lang/Integer;" + lineFeed(1)
    contents += "invokevirtual java/util/ArrayList set (ILjava/lang/Object;)Ljava/lang/Object;" + lineFeed(1)
    contents += "pop" + lineFeed(1)
    contents += "aload " + hStoreTemp._2 + lineFeed(1)
    //cast
    contents += "checkcast java/lang/Integer" + lineFeed(1)
    contents += "invokevirtual java/lang/Integer intValue ()I" + lineFeed(1)
    contents += hStoreHead._1 + lineFeed(1)

    //decrease the pointing number to iterate
    contents += "iinc " + nthCounterReferenceNumber + " -1" + lineFeed(1)
    contents += "goto " + labelInner + lineFeed(1)


    //contents += hIf2._2
    //if inner false (means 0, means the list value is larger than the head), just iterate.
    contents += hIf2._3

    contents += "iinc " + nthCounterReferenceNumber + " -1" + lineFeed(1)
    contents += "goto " + labelInner + lineFeed(1)



    //if inner true.
    contents += "aload " + newListVariableNumber + lineFeed(1)
    contents += "iload " + hStoreHead._2 + lineFeed(1)
    contents += "invokestatic java/lang/Integer valueOf (I)Ljava/lang/Integer;" + lineFeed(1)
    contents += "invokevirtual java/util/ArrayList add (Ljava/lang/Object;)Z" + lineFeed(1)
    contents += "pop" + lineFeed(1)
    contents += "goto " + labelOuter + lineFeed(1)

    contents += hIf2._4

    //**************************** Inner loop End


    contents += hIf1._2
    contents += hIf1._3

    //add the last remained value in the rest list to the new list
    contents += "aload " + newListVariableNumber + lineFeed(1)
    contents += "iload " + hStoreHead._2 + lineFeed(1)
    contents += "invokestatic java/lang/Integer valueOf (I)Ljava/lang/Integer;" + lineFeed(1)
    contents += "invokevirtual java/util/ArrayList add (Ljava/lang/Object;)Z" + lineFeed(1)
    contents += "pop" + lineFeed(1)

    //if outer true(zero)
    contents += "aload " + newListVariableNumber + lineFeed(1)

    contents += hIf1._4

    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
    contents
  }

  //*********************************** Function End

  //*********************************** Function Helper Start

  //Used by List and Sort
  def helperNewList: (ListBuffer[String], Int) = {
    val result = new ListBuffer[String]

    result += "new java/util/ArrayList" + lineFeed(1)
    result += "dup" + lineFeed(1)
    result += "invokespecial java/util/ArrayList <init> ()V" + lineFeed(1)

    val hStore = helperStore("a", "list")
    result += hStore._1
    val variableNumber = hStore._2

    (result, variableNumber)
  }

  def helperStore(inputType: String, inputName: String): (String, Int) = {
    val numberOfArgs = variableTable.size
    val variableNumber = {
      if (!numberOfArgs.equals(0))
        numberOfArgs
      else
        0
    }
    val result = inputType + "store " + variableNumber + lineFeed(1)
    val info = (inputName + variableNumber -> variableNumber)
    variableTable += info
    (result, variableNumber)
  }

  def helperComparison(input: String): ListBuffer[String] = {
    val result = new ListBuffer[String]

    val comparatorName = input

    val comparator = {
      //Between two vaules
      if (comparatorName.equals("equal")) {
        "if_icmpeq "
      }
      else if(comparatorName.equals("greater")) {
        "if_icmpgt "
      }
      else if(comparatorName.equals("lesser")) {
        "if_icmplt "
      }
      //Take one value and compare to zero
      else if(comparatorName.equals("equalzero")) {
        "ifeq "
      }
      else if(comparatorName.equals("lesszero")) {
        "iflt "
      }
    }

    val labelStart = labelManager(comparatorName.toUpperCase())
    val labelStop = labelManager("STOP"+comparatorName.toUpperCase)

    result += comparator + labelStart + lineFeed(1)
    result += "iconst_0" + lineFeed(1)
    result += "goto " + labelStop + lineFeed(1)
    result +=  labelStart + ":" + lineFeed(1)
    result += "iconst_1" + lineFeed(1)
    result += labelStop + ":" + lineFeed(2)

    result
  }

  def helperIf(input: String): (String, String, String, String) = {
    val labelFalse = labelManager("FALSE")
    val labelEscapeif = labelManager("ESCAPEIF")

    val equalType = {
      if (input.equals("equal"))
        "ifeq "
      else if (input.equals("notequal"))
        "ifne "
      else if (input.equals("lesszero"))
        "iflt "
    }

    val valueIfeq = equalType + labelFalse + lineFeed(1)
    val valueGotoEscape = "goto " + labelEscapeif + lineFeed(1)
    val valueLabelTrue = labelFalse + ":" + lineFeed(1)
    val valueLabelEscapeif = labelEscapeif + ":" + lineFeed(2)

    (valueIfeq, valueGotoEscape, valueLabelTrue, valueLabelEscapeif)
  }

  def helperNth(): String = {
    val result = "invokevirtual java/util/ArrayList get (I)Ljava/lang/Object;" + lineFeed(1)
    result
  }

  //*********************************** Function Helper End



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
      case "println" => funcPrintln(secondInput) //**Only function doesn't return any value within this compiler. It could be a problem if this is used in defn. Defn assumes any user function returns a integer value.
      case "list" => funcList(secondInput)
      case "nth" => funcListNth(secondInput)
      case "rest" => funcRest(secondInput)
      case "sort" => funcSort(secondInput)
    }
  }

  //*********************************** Single Value start
  def singleInt(input: String): String = {
    val contentsForBodyWriter = new ListBuffer[String]
    var contentsForReturn = ""

    val numResult = numberRange(input.toInt)
    contentsForBodyWriter += numResult
    contentsForReturn = numResult

    if(functionSwitch._1.equals(0) & userFunctionSwitch._1.equals(0)) {
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
      case Vector(a) => deriveValueFromVector(a)
      case Argument(a) => deriveValueFromVector(a)
      case Function(a, b) => functionSelector(a, b)
      case UserFunction(a, b) => userFunction(a, b)
    }
  }

  def bodyWriter(input: ListBuffer[String]): Unit = {
    if(userFunctionSwitch._1.equals(0)) {
      for(i <- input)
        body += "   " + i
    }
  }

  def userFunction(firstInput: Expr, secondInput: Argument): ListBuffer[String] = {
    var nested = 0
    val previousSwitch = functionSwitch

    val contents = new ListBuffer[String]
    val name = firstInput match {
      case Value(a) => a
    }

    val args = secondInput match {
      case Argument(a) => a
    }

    if (functionSwitch._1.equals(1)) {
      nested = 1
    }

    functionSwitch = (1, "userFunction")

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

    //Efficient code only works if the recursive call is at the second depth(only one parent function).
    //It shouldn't be with some functions: Plus, Minus, Multiply, Remainder, Or, Equal.
    if(name.equals(userFunctionSwitch._2)
      && numberOfRecursiveCall <= 1
      && !functionSwitch._2.equals("plus")
      && !functionSwitch._2.equals("minus")
      && !functionSwitch._2.equals("multi")
      && !functionSwitch._2.equals("rem")
      && !functionSwitch._2.equals("or")
      && !functionSwitch._2.equals("equal")) {
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

    if(nested.equals(0))
      bodyWriter(contents)
    functionSwitch = previousSwitch
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

  def addLocalVariable(firstInput: String, secondInput: String): Unit = {
    val functionName = firstInput
    val variableName = secondInput

    //println("add Local Variable: " + functionName + " " + variableName)

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
      //println("get Local Variable: " + functionName + " " + variableName)
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
    val previousSwitch = functionSwitch
    val nested = {
      if (functionSwitch._1.equals(1))
        1
      else
        0
    }
    functionSwitch = (1, "println")

    //Main parts will be added first than the getstatic below, therefore bodyWriter should come just after the getstatic line.
    contents += "getstatic java/lang/System out Ljava/io/PrintStream;" + lineFeed(2)

    if(nested.equals(0)) {
      bodyWriter(contents)
      //Before going to bodyWriter again, getstatic should be removed first or it gets duplication.
      contents -= "getstatic java/lang/System out Ljava/io/PrintStream;" + lineFeed(2)
    }

    val result = input match {
      case Argument(arg) => evalExpression(arg.head)
    }
    //Type check if the result is String or ListBuffer. It could be a separate function to also support other functions.
    if (result.isInstanceOf[ListBuffer[String]]) {
      for (i <- result.asInstanceOf[ListBuffer[String]])
        contents += i
    } else
      contents += result.toString


    contents += "invokevirtual java/io/PrintStream println (" + outParam + ")V" + lineFeed(2)

    //In case println is used in defn, defn should return a integer value but println originally doesn't return a value.
    //So next two lines force Println put a interger value into the stack to avoid the error.
    if(nested.equals(1))
      contents += "iconst_0" + lineFeed(1)

    functionSwitch = previousSwitch

    if(nested.equals(0))
      bodyWriter(contents)
    contents
  }
}