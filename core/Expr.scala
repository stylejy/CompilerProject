import scala.collection.immutable

/**
  * Created by stylejy on 19/11/2015.
  */
sealed trait Expr
  case class Value(value: String) extends Expr
  case class Keyword(value: String) extends Expr

  case class Function(firstValue: Expr, secondValue: Argument) extends Expr
  case class UserFunction(firstValue: Expr, secondValue: Argument) extends Expr
  case class Argument(arguments: immutable.Seq[Expr]) extends Expr
  case class Call(firstValue: Expr, secondValue: Argument) extends Expr

  case class List(fields: immutable.Seq[Expr]) extends Expr
  case class Vector(fields: immutable.Seq[Expr]) extends Expr
  case class Map(fields: immutable.Seq[Expr]) extends Expr
  case class Set(fields: immutable.Seq[Expr]) extends Expr

  case class Addition(firstValue: Expr, secondValue: Expr) extends Expr
  case class Substraction(firstValue: Expr, secondValue: Expr) extends Expr
  case class Multiplication(firstValue: Expr, secondValue: Expr) extends Expr
  case class Division(firstValue: Expr, secondValue: Expr) extends Expr
  case class Remainder(firstValue: Expr, secondValue: Expr) extends Expr