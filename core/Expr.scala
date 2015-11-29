/**
  * Created by stylejy on 19/11/2015.
  */
sealed trait Expr
  case class Value(value: String) extends Expr
  case class Symbol(firstValue: Expr, secondValue: Expr, thirdValue: Expr) extends Expr
  case class Addition(firstValue: Expr, secondValue: Expr) extends Expr
  case class Substraction(firstValue: Expr, secondValue: Expr) extends Expr
  case class Multiplication(firstValue: Expr, secondValue: Expr) extends Expr
  case class Division(firstValue: Expr, secondValue: Expr) extends Expr
  case class Remainder(firstValue: Expr, secondValue: Expr) extends Expr