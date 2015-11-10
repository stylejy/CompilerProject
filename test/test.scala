/**
  * Created by stylejy on 07/11/2015.
  * Thanks GOD for all.
  */

object test {
  def main(args: Array[String]) {
    //Evaluate the argument of Calculator
    val result = new TestParser("(prin(t)ln)").InputLine.run()
    if (result.isSuccess)
      println("Yes :D, it is " + result)
    else
      println("fail")

  }
}
