import org.parboiled2.ParseError

import scala.util.{Failure, Success}

/**
  * Created by stylejy on 07/11/2015.
  * Thanks GOD for all.
  */

object test {
  def main(args: Array[String]) {
    //Evaluate the argument of Calculator

    val result = new ParsingRules("defn a (+ 2 5)")
    result.InputLine.run() match {
      case Success(tree) =>
        println("Tree: " + tree)
        //println("Result: " + result.eval(tree) + "\nTree: " + tree)
        //val test = new ByteCodeGenerator("test", tree)
        //test.writer
      case Failure(e: ParseError) => println("Expression is not valid")
      case Failure(e) => println("Unexpected error")
    }
  }
}
