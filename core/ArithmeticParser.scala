
import org.parboiled2._

import scala.collection.mutable.ListBuffer

/**
  * Created by stylejy on 09/11/2015.
  * Thanks GOD for all.
  */

//Very simple matching test roule
class ArithmeticParser(val input: ParserInput) extends Parser {
  var list = new ListBuffer[Object]()

  sealed trait Expr
  case class Value(value: String) extends Expr
  case class Addition(firstValue: Expr, secondValue: Expr) extends Expr
  case class Substraction(firstValue: Expr, secondValue: Expr) extends Expr
  case class Multiplication(firstValue: Expr, secondValue: Expr) extends Expr
  case class Division(firstValue: Expr, secondValue: Expr) extends Expr
  case class Remainder(firstValue: Expr, secondValue: Expr) extends Expr

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

  def eval(expr: Expr): Int =
    expr match {
      case Value(v) =>
        list += v
        v.toInt
      case Addition(a, b) =>
        list += ("Add")
        eval(a) + eval(b)
      case Substraction(a, b) =>
        list += ("Sub")
        eval(a) - eval(b)
      case Multiplication(a, b) =>
        list += ("Mul")
        eval(a) * eval(b)
      case Division(a, b) =>
        list += ("Div")
        eval(a) / eval(b)
      case Remainder(a, b) =>
        list += ("Rem")
        eval(a) % eval(b)
    }
}

