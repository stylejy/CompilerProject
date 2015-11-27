
import org.parboiled2._

/**
  * Created by stylejy on 09/11/2015.
  * Thanks GOD for all.
  */

//Very simple matching test roule
class ParsingRules(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI}

  def Expression: Rule1[Expr] = rule {
    Term
  }

  def Term = rule {
    Factor
  }

  def Factor = rule { Number | String | Parens | SimpleArithmetic }

  def String = rule { "\"" ~ Characters ~ "\"" }

  def Characters = rule { capture(zeroOrMore(CharPredicate.AlphaNum)) ~> Value }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Number = rule { capture((""|"+"|"-") ~ oneOrMore(CharPredicate.Digit)) ~> Value }

  def SimpleArithmetic = rule { '+' ~ (' ' ~ (Expression ~ ' ' ~ Expression)) ~> Addition | '-' ~ (' ' ~ (Expression ~ ' ' ~ Expression)) ~> Substraction | '*' ~ (' ' ~ (Expression ~ ' ' ~ Expression)) ~> Multiplication | '/' ~ (' ' ~ (Expression ~ ' ' ~ Expression)) ~> Division | '%' ~ (' ' ~ (Expression ~ ' ' ~ Expression)) ~> Remainder }

  //For testing
  def eval(expr: Expr): Int =
    expr match {
      case Value(v) => {
        try {
          v.toInt
        } catch {
          case e: NumberFormatException => {
            println(v)
            return 0
          }
        }
        v.toInt
      }
      case Addition(a, b) => eval(a) + eval(b)
      case Substraction(a, b) => eval(a) - eval(b)
      case Multiplication(a, b) => eval(a) * eval(b)
      case Division(a, b) => eval(a) / eval(b)
      case Remainder(a, b) => eval(a) % eval(b)
    }
}

