import java.io._

import scala.collection.mutable.ListBuffer

/**
  * Created by stylejy on 18/11/2015.
  * Thanks GOD for all.
  */
class TGByteCodeGenerator(classname: String, ast: Expr) {

  val pw = new PrintWriter(new File(classname+".tg"))

  def astManager(expr: Expr): Unit =
    expr match {
      case Value(v) =>
        pw.write("iconst_" + v.toInt + "\n")
      case Addition(a, b) =>
        astManager(a)
        astManager(b)
        pw.write("iadd\n")
      case Substraction(a, b) =>
        astManager(a)
        astManager(b)
        pw.write("isub\n")
      case Multiplication(a, b) =>
        astManager(a)
        astManager(b)
        pw.write("imul\n")
      case Division(a, b) =>
        astManager(a)
        astManager(b)
        pw.write("idiv\n")
      case Remainder(a, b) =>
        astManager(a)
        astManager(b)
        pw.write("irem\n")
    }


  def writer = {

    pw.write(".class public "+classname+"\n")
    pw.write(".super java/lang/Object\n\n")

    pw.write(".method public static main : ([Ljava/lang/String;)V\n")
    pw.write(".limit stack 10\n")
    pw.write(".limit locals 10\n\n")

    pw.write("getstatic java/lang/System out Ljava/io/PrintStream;\n\n")

    astManager(ast)

    pw.write("invokevirtual java/io/PrintStream println (I)V\n")

    pw.write("\nreturn\n")
    pw.write(".end method")

    pw.close()
  }
}
