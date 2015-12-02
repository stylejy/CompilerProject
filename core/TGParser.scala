
import org.parboiled2._

/**
  * Created by stylejy on 09/11/2015.
  * Thanks GOD for all.
  */

//Very simple matching test roule
class TGParser(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI}

  def Expression: Rule1[Expr] = rule {
    zeroOrMore(' ') ~ Term ~ zeroOrMore(' ')
  }

  def Term: Rule1[Expr] = rule {
    Factor
  }

  def Factor = rule { Parens | Vectors | Functions | Userfunctions | String | Characters | Numbers }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Arguments = rule { oneOrMore(Term).separatedBy(' ') ~> Argument }

  def String = rule { "\"" ~ Characters ~ "\"" }

  def Characters = rule { capture(zeroOrMore(CharPredicate.AlphaNum)) ~> Value }

  def Numbers = rule { capture(( "+" | "-" | "" ) ~ oneOrMore(CharPredicate.Digit) ~ ( "." | "/" | "") ~ zeroOrMore(CharPredicate.Digit)) ~> Value }

  def Functions = rule { Keywords ~ ' ' ~ Arguments ~> Function }

    def Keywords = rule { capture( "defn" | "def" | "if" | "and" | "or" | '=' | '+' | '-' ) ~> Keyword }

  def Vectors = rule { '[' ~ oneOrMore(Characters).separatedBy(' ') ~ ']' ~> Vector }

  //Every function should be with parenthesis, so the rule with the parens below works without interfering with the other factors
  def Userfunctions = rule { '(' ~ Characters ~ ' ' ~ Arguments ~ ')' ~> UserFunction }



  /*
  def Arithmetic = rule { '+' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Addition | '-' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Substraction | '*' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Multiplication | '/' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Division | '%' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Remainder }

  def Lists = rule { '(' ~ oneOrMore(Characters).separatedBy(' ') ~ ')' ~> List }

  def Maps = rule { '{' ~ oneOrMore(Characters).separatedBy(' ') ~ '}' ~> Map }

  def Sets = rule { "#{" ~ oneOrMore(Characters).separatedBy(' ') ~ '}' ~> Set }
  */


  /*
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
    */
}

