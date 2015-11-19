
import org.parboiled2._

/**
  * Created by stylejy on 09/11/2015.
  * Thanks GOD for all.
  */

//Very simple matching test roule
class ArithmeticParser(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI}

  def Expression: Rule1[Expr] = rule {
    '(' ~ Term ~ ')'
  }

  def Term: Rule1[Expr] = rule {
    Factor
  }

  def Factor = rule { Number | Parens | SimpleArithmetic }

  def Parens = rule { '(' ~ Term ~ ')' }

  def Number = rule { capture(oneOrMore(CharPredicate.Digit)) ~> Value }

  def SimpleArithmetic = rule { '+' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Addition | '-' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Substraction | '*' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Multiplication | '/' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Division | '%' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Remainder }

  //For testing
  def eval(expr: Expr): Int =
    expr match {
      case Value(v) => v.toInt
      case Addition(a, b) => eval(a) + eval(b)
      case Substraction(a, b) => eval(a) - eval(b)
      case Multiplication(a, b) => eval(a) * eval(b)
      case Division(a, b) => eval(a) / eval(b)
      case Remainder(a, b) => eval(a) % eval(b)
    }
}

