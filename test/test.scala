import org.parboiled2.ParseError

import scala.util.{Failure, Success}

/**
  * Created by stylejy on 07/11/2015.
  * Thanks GOD for all.
  */

object test {
  def main(args: Array[String]) {
    //Evaluate the argument of Calculator

    val result = new ArithmeticParser(args.head)
    result.InputLine.run() match {
      case Success(tree) =>
        println("Result: " + result.eval(tree) + "\nTree: " + tree)
        println(result.list.mkString(", "))
        val test = new ByteCodeGenerator(args.last, result.list)
        test.writer
      case Failure(e: ParseError) => println("Expression is not valid")
      case Failure(e) => println("Unexpected error")
    }
  }
}
