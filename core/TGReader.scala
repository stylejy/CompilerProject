import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success}

/**
  * Created by stylejy on 24/01/2016.
  */
class TGReader {
  val vTable = new TGVariableSymbolTable
  val fTable = new TGFunctionSymbolTable

  val source = Source.fromFile("test.tg")
  for(line <- source.getLines()) {
    println(line)
    val treeGen = new TGParser(line)
    treeGen.InputLine.run() match {
      case Success(tree) =>
        println("Tree: " + tree)
        //println("Result: " + result.eval(tree) + "\nTree: " + tree)
        //val test = new ByteCodeGenerator("test", tree)
        //test.writer
        val result = new TGTreeEvaluator().evalExpression(tree, vTable, fTable)
        if(result.isInstanceOf[Unit])
          println("RESULT =============> Done!")
        else
          println("RESULT =============> " + result)
      case Failure(e: ParseError) => println("Expression is not valid")
      case Failure(e) => println("Unexpected error")
  }
  }
}
