
import org.parboiled2._

import scala.collection.immutable
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

  def Factor = rule { Booleans | List | Parens | Vectors | Functions | Userfunctions | IntNumbers | Characters }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Arguments = rule { oneOrMore(Term).separatedBy(' ') ~> Argument }

  def Characters = rule { capture(zeroOrMore(CharPredicate.AlphaNum)) ~> Value }

  def IntNumbers = rule { capture(( "+" | "-" | "" ) ~ oneOrMore(CharPredicate.Digit)) ~> IntNumber }

  def Functions = rule { Keywords ~ ' ' ~ Arguments ~> Function }

    def Keywords = rule { capture( "defn" | "def" | "if" | "and" | "or" | '=' | '+' | '-' | '*'| "rem" | "println" | "list" | "nth" | "rest" | "sort" ) ~> Keyword }

  def Vectors = rule { '[' ~ oneOrMore(Characters).separatedBy(' ') ~ ']' ~> Vector }

  def Booleans = rule { capture( "false" | "true" ) ~> Bool }

  def List = rule { "'(" ~ oneOrMore(Term).separatedBy(' ') ~ ')' ~> ListQuote }

  //Every function should be with parenthesis, so the rule with the parens below works without interfering with the other factors
  def Userfunctions = rule { '(' ~ Characters ~ ' ' ~ Arguments ~ ')' ~> UserFunction }

}

