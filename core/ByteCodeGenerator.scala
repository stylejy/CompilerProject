import java.io._

import scala.collection.mutable.ListBuffer

/**
  * Created by stylejy on 18/11/2015.
  * Thanks GOD for all.
  */
class ByteCodeGenerator(classname: String, ASTList: ListBuffer[Object]) {

  def writer = {

    val pw = new PrintWriter(new File(classname+".tg"))

    pw.write(".class public "+classname+"\n")
    pw.write(".super java/lang/Object\n\n")

    pw.write(".method public static main : ([Ljava/lang/String;)V\n")
    pw.write(".limit stack 10\n")
    pw.write(".limit locals 10\n\n")

    pw.write("getstatic java/lang/System out Ljava/io/PrintStream;\n\n")

    ASTList.head match {
      case "Add" =>
        for (i <- ASTList.reverse) {
          if (i != "Add")
            pw.write("iconst_"+i+"\n")
          else
            pw.write("iadd\n")
        }
        pw.write("\ninvokevirtual java/io/PrintStream println (I)V\n")
    }

    pw.write("return\n")
    pw.write(".end method")

    pw.close()
  }
}
