import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success}

/**
  * Created by stylejy on 24/01/2016.
  */
class TGController {
  val vTable = new TGVariableSymbolTable
  val fTable = new TGFunctionSymbolTable

  val file = "test.tg"
  val source = Source.fromFile(file)
  //Split the pure name without its type to pass the name as the class name in JVM code.
  val name = file.split('.')(0)
  val generator = new TGCodeGenerator(name)

  for(line <- source.getLines()) {
    println(line)
    val treeGen = new TGParser(line)
    treeGen.InputLine.run() match {
      case Success(tree) =>
        println("Tree: " + tree)
        generator.run(tree)
        try {
          val result = new TGEvaluator().evalExpression(tree, vTable, fTable)
          if (result.isInstanceOf[Unit])
            println("RESULT =============> Done!")
          else
            println("RESULT =============> " + result)
        } catch {
          case ex: StackOverflowError => println("StackOverflowError by the interpreter")
        }
      case Failure(e: ParseError) => println("Expression is not valid")
      case Failure(e) => println("Unexpected error")
    }
  }
  generator.composer
}
