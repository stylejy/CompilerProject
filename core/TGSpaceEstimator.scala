import scala.collection.immutable

/**
  * Created by stylejy on 20/02/2016.
  */
class TGSpaceEstimator {

  var localSize = 0
  var stackSize = 0
  var numOfList = 0 //Indicates how many lists are processed.
  var nestedListSwitch = 0 //Once a list is nested in another list, it is activated until the current list finishes.

  def run(input: Expr): (Int, Int) = {
    val result = expression(input)
    val finalLocalSize = result._1
    val finalStackSize = result._2
    //clear current size for other codes or gets wrong size
    localSize = 0
    stackSize = 0
    numOfList = 0
    nestedListSwitch =0
    (finalLocalSize, finalStackSize)
  }

  def expression(expr: Expr): (Int, Int) = {
    expr match {
      case Value(a) => returnResult(localSize, 1)
      case Keyword(a) => returnResult(localSize, 1)
      case IntNumber(a) => returnResult(localSize, 1)
      case Bool(a) => returnResult(localSize, 1)
      case Sentence(a) => returnResult(localSize, 1)
      case Argument(a) => returnResult(localSize, stackSize)//it doesn't affect the stack size.
      case Vector(a) => vector(a)
      case Function(a, b) => function(a, b)
      case UserFunction(a, b) =>
        b match {
          case Argument(group) =>
            val result = user(group)
            //println("UserFunction Result "+result)
            returnResult(result._1, result._2)
        }
      case _ => (localSize, stackSize)
    }
  }

  def user(input: immutable.Seq[Expr]): (Int, Int) = {
    var largestStackSize = 0
    for(i <- input) {
      val currentStackValue = expression(i)._2
      if(currentStackValue > largestStackSize)
        largestStackSize = currentStackValue
    }
    //The safest stack size is to add (the largest stack size) to (the number of elements minus 1)
    var result = (localSize, largestStackSize + (input.size - 1))
    if(result._2 < stackSize)
      result = (localSize, stackSize)
    result
  }

  def vector(input: immutable.Seq[Expr]): (Int, Int) = {
    var result = (localSize, stackSize)
    for(i <- input) {
      val temp = expression(i)
      if(temp._1 > localSize || temp._2 > stackSize)
        result = temp
    }
    result
  }

  def returnResult(localSizeInput: Int, stackSizeInput: Int): (Int, Int) = {
    val localResult =
      if(localSizeInput > localSize)
        localSizeInput
      else
        localSize

    val stackResult =
      if(stackSizeInput > stackSize)
        stackSizeInput
      else
        stackSize

    (localResult, stackResult)
  }

  def function(firstInput: Expr, secondInput: Argument): (Int, Int) = {
    val keyword = firstInput match {
      case Keyword(a) => a.toString
    }

    keyword match {
      case "defn" =>
        secondInput match {
          case Argument(group) =>
            group(1) match {
              case Vector(a) => localSize = a.size
            }
            expression(group(2))
        }
      case "if" =>
        secondInput match {
          case Argument(group) =>
            val result1 = expression(group(0))
            //println("if result1 " + result1)
            val result2 = expression(group(1))
            //println("if result2 " + result2)
            val result3 = expression(group(2))
            //println("if result3 " + result3)
            if(result2._1 > result3._1 || result2._2 > result3._2) {
              if (result1._1 > result2._1 || result1._2 > result2._2)
                returnResult(result1._1, result1._2)
              else
                returnResult(result2._1, result2._2)
            }
            else {
              if (result1._1 > result3._1 || result1._2 > result3._2)
                returnResult(result1._1, result1._2)
              else
                returnResult(result3._1, result3._2)
            }
        }
      case "or" => returnResult(localSize, 1)
      case "=" | "+" | "-" | "*" | "rem" =>
        secondInput match {
          case Argument(group) =>
            val result1 = expression(group(0))
            //println("ari result1 " + result1)
            val result2 = expression(group(1))
            //println("ari result2 " + result2)

            //Need to describe
            val stackResult =
              if((result1._2 > result2._2 || result1._2 == result2._2 ) && result1._2 > 1) {
                //why plus 1? In function One + function Two, left function ends up getting one result consuming just one stack.
                //Then, consider how many space function Two needs.
                //So the result should be one result which is larger than the other plus 1.
                result1._2 + 1
              } else if(result1._2 < result2._2 && result2._2 > 1) {
                result2._2 + 1
              } else {
                //stack should be at least 2
                2
              }

            //println("ari stackResult: " + stackResult)
            returnResult(localSize, stackResult)
        }
      case "println" =>
        secondInput match {
          case Argument(group) =>
            val result = expression(group.head)
            //println("println Result" + result)

            //getstatic needs a 1 stack space for Int
            returnResult(result._1, result._2 + 1)
        }
      case "list" =>
        numOfList += 1
        localSize += 1
        if(numOfList > 1)
          nestedListSwitch = 1
        val result = secondInput match {
          case Argument(group) =>
            var largestValue = (0, 0)
            for (i <- group) {
              val value = expression(i)
              if(value._1 > largestValue._1 || value._2 > largestValue._2)
              largestValue = value
            }
            largestValue
        }
        if(nestedListSwitch.equals(1)) {
          numOfList -= 1
          return (result._1, result._2 + 1)
        }
        else if(numOfList.equals(1)) {
          numOfList -= 1
          return (result._1, 2)
        }
        (localSize, stackSize)
      case "nth" =>
        secondInput match {
          case Argument(group) =>
            expression(group(0))
        }
      case "sort" => returnResult(5, 4) //needs tightening the space

      case _ => (localSize, stackSize)
    }
  }
}
