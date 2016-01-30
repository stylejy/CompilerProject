import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success}

/**
  * Created by stylejy on 24/01/2016.
  */
class TGReader {
  val source = Source.fromFile("test.tg")
  val line = try source.getLines() mkString "\n" finally source.close()

  //print the whole source code
  println(line)

  val code = new TGParser(line)
  code.InputLine.run() match {
    case Success(tree) =>
      println("Tree: " + tree)
      //println("Result: " + result.eval(tree) + "\nTree: " + tree)
      //val test = new ByteCodeGenerator("test", tree)
      //test.writer
      new TGTreeEvaluator(new TGUserFunctionList, new InformationStructure("ROOT", Empty(0), tree, 0)).run
    case Failure(e: ParseError) => println("Expression is not valid")
    case Failure(e) => println("Unexpected error")
  }
}
