
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

  def Factor = rule { Booleans | Parens | Vectors | Functions | Userfunctions | Strings | IntNumbers | Characters }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Arguments = rule { oneOrMore(Term).separatedBy(' ') ~> Argument }

  def Strings = rule { "\"" ~ Characters ~ "\"" ~> Sentence }

  def Characters = rule { capture(zeroOrMore(CharPredicate.AlphaNum)) ~> Value }

  def IntNumbers = rule { capture(( "+" | "-" | "" ) ~ oneOrMore(CharPredicate.Digit)) ~> IntNumber }

  def Functions = rule { Keywords ~ ' ' ~ Arguments ~> Function }

    def Keywords = rule { capture( "defn" | "def" | "if" | "and" | "or" | '=' | '+' | '-' | '*'| "rem" ) ~> Keyword }

  def Vectors = rule { '[' ~ oneOrMore(Characters).separatedBy(' ') ~ ']' ~> Vector }

  def Booleans = rule { capture( "false" | "true" ) ~> Bool }

  //Every function should be with parenthesis, so the rule with the parens below works without interfering with the other factors
  def Userfunctions = rule { '(' ~ Characters ~ ' ' ~ Arguments ~ ')' ~> UserFunction }



  /*
  def Arithmetic = rule { '+' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Addition | '-' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Substraction | '*' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Multiplication | '/' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Division | '%' ~ (' ' ~ (Term ~ ' ' ~ Term)) ~> Remainder }

  def Lists = rule { '(' ~ oneOrMore(Characters).separatedBy(' ') ~ ')' ~> List }

  def Maps = rule { '{' ~ oneOrMore(Characters).separatedBy(' ') ~ '}' ~> Map }

  def Sets = rule { "#{" ~ oneOrMore(Characters).separatedBy(' ') ~ '}' ~> Set }
  */
}

