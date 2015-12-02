import org.parboiled2.ParseError

import scala.util.{Failure, Success}

/**
  * Created by stylejy on 07/11/2015.
  * Thanks GOD for all.
  */

object test {
  def main(args: Array[String]) {
    //Evaluate the argument of Calculator

    /* fibonacci in Clojure
    //(defn fib [x] (if (or (= x 0) (= x 1)) x (+ (fib (- x 2)) (fib (- x 1)))))
    */

    val result = new TGParser("(defn fib [x] (if (or (= x 0) (= x 1)) x (+ (fib x) (fib y))))")
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
