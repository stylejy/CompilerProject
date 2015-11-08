/**
  * Created by stylejy on 07/11/2015.
  */
import org.parboiled2._

object test {
  def main(args: Array[String]) {
    println("test")
    //Evaluate the argument of Calculator
    println(new Calculator("1+(2-3*4)/5").InputLine.run())
  }

  class Calculator(val input: ParserInput) extends Parser {
    def InputLine = rule { Expression ~ EOI }

    def Expression: Rule1[Int] = rule {
      Term ~ zeroOrMore(
        '+' ~ Term ~> ((_: Int) + _)
          | '-' ~ Term ~> ((_: Int) - _))
    }

    def Term = rule {
      Factor ~ zeroOrMore(
        '*' ~ Factor ~> ((_: Int) * _)
          | '/' ~ Factor ~> ((_: Int) / _))
    }

    def Factor = rule { Number | Parens }

    def Parens = rule { '(' ~ Expression ~ ')' }

    def Number = rule { capture(Digits) ~> (_.toInt) }

    def Digits = rule { oneOrMore(CharPredicate.Digit) }
  }



}
