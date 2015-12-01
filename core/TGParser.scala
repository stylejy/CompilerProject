
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

  def Factor = rule { Parens | Lists | Vectors | Maps | Sets | Functions | Symbols | Number | String | Arithmetic | Characters }

  def String = rule { "\"" ~ Characters ~ "\"" }

  def Characters = rule { capture(zeroOrMore(CharPredicate.AlphaNum)) ~> Value }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Number = rule { capture(( "+" | "-" | "" ) ~ oneOrMore(CharPredicate.Digit) ~ ( "." | "/" | "") ~ zeroOrMore(CharPredicate.Digit)) ~> Value }

  def Arithmetic = rule { '+' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Addition | '-' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Substraction | '*' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Multiplication | '/' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Division | '%' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Remainder }

  def Symbols = rule { Vars ~ ' ' ~ Characters ~ ' ' ~ Expression ~> Symbol }

    def Vars = rule { capture("def") ~> Value }

  def Functions = rule { Creators ~ ' ' ~ Characters ~ Expression ~ '(' ~ Expression ~ ')' ~> Function }

    def Creators = rule { capture("defn") ~> Value }

  def Lists = rule { '(' ~ oneOrMore(Characters).separatedBy(' ') ~ ')' ~> List }

  def Vectors = rule { '[' ~ oneOrMore(Characters).separatedBy(' ') ~ ']' ~> Vector }

  def Maps = rule { '{' ~ oneOrMore(Characters).separatedBy(' ') ~ '}' ~> Map }

  def Sets = rule { "#{" ~ oneOrMore(Characters).separatedBy(' ') ~ '}' ~> Set }



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

