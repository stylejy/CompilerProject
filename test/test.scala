import org.parboiled2.ParseError

import scala.util.{Failure, Success}

/**
  * Created by stylejy on 07/11/2015.
  * Thanks GOD for all.
  */

object test {
  def main(args: Array[String]) {
    //Evaluate the argument of Calculator
    val result = new TestParser("(+ 2 2)")
    result.InputLine.run() match {
      case Success(tree) => println("Result: " + result.eval(tree) + "\nTree: " + tree)
      case Failure(e: ParseError) => println("Expression is not valid")
      case Failure(e) => println("Unexpected error")
    }


  }
}
