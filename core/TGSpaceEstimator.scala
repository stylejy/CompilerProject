/**
  * Created by stylejy on 20/02/2016.
  */
class TGSpaceEstimator {

  var localSize = 0

  def run(input: Expr): (Int, Int) = {
    val stackSize = expression(input)

    val localResult = localSize
    localSize = 0
    (localResult,stackSize)
  }

  def expression(expr: Expr): Int = {
    expr match {
      case Value(a) => 1
      case Keyword(a) => 1
      case IntNumber(a) => 1
      case Bool(a) => 1
      case Sentence(a) => 1
      case Argument(a) => 0
      case Vector(a) =>
        var result = 0
        for (i <- a) {
          result += expression(i)
        }
        result
      case Function(a, b) => function(a, b)
      case UserFunction(a, b) =>
        b match {
          case Argument(group) =>
            var result = 0
            for (i <- group) {
              result += expression(i)
            }
            result
        }
      case ListQuote(a) => 0 //should change
    }
  }

  def function(firstInput: Expr, secondInput: Argument): Int = {
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
            expression(group(0)) +
              (if(expression(group(1)) > expression(group(2)))
                expression(group(1))
                else
                expression(group(2))
                )
        }
      case "or" => 1
      case "=" | "+" | "-" | "*" | "rem" =>
        secondInput match {
          case Argument(group) =>
            expression(group(0)) + expression(group(1))
        }
      case "println" =>
        secondInput match {
          case Argument(group) =>
            var result = 0
            for (i <- group) {
              //getstatic needs a 1 stack space for Int
              result += expression(i) + 1
            }
            result
        }
      case _ => 0
    }
  }
}
