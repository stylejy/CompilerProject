/**
  * Created by stylejy on 07/11/2015.
  * Thanks GOD for all.
  */

object test {
  def main(args: Array[String]) {
    //Evaluate the argument of Calculator
    if (new TestParser("(println)").InputLine.run().isSuccess)
      println("Yes, it is success :D")
    else
      println("fail")

  }
}
