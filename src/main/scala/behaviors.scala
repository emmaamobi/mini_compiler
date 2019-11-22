package edu.luc.cs.laufer.cs473.expressions

import ast._
import scala.util.{Failure, Success, Try}

object behaviors {

  case class Cell(var value: Value) {
    def get: Value = value
    def set(value: Value): Unit = this.value = value
  }
  object Cell {
    def apply(i: Int): Cell = Cell(Num(i))
    val NULL = Cell(0)
  }

  type Instance = Map[String, Cell]
  type Store = Instance
  sealed trait Value
  case class Num(value: Int) extends Value
  type Result = Try[Cell]

  def lookup(store: Store)(name: String): Result =
    store.get(name).fold {
      Failure(new NoSuchFieldException(name)): Result
    } {
      Success(_)
    }

  def evaluate(m: Store)(e: Expr): Result = e match { //TODO for 3b
    case Constant(c) => Success(Cell(Num(c)))
    case UMinus(r)   => evalUnary(m)(r , "-")
    case Plus(l, r)  => evalSigns(m)(l, "+", r)
    case Minus(l, r) => evalSigns(m)(l, "-", r)
    case Times(l, r) => evalSigns(m)(l, "*", r)
    case Div(l, r)   => evalSigns(m)(l, "/", r)
    case Mod(l, r)   => evalSigns(m)(l, "%", r)
    case Var(v) => lookup(m)(v)
    case Loop(l,r) => ???
    case Assignment(l, r) => ???
    case Block(s@_*) => ???
    case Conditional(e,l,r) => ???
  }
  def evalUnary(m: Store)(v : Expr, sign : String) : Result = {
    val v1 = evaluate(m)(v)
    v1 match {
      case (Success(Cell(Num(v1)))) => {
        case "+" => Success(Cell(Num(v1)))
        case "-" => Success(Cell(Num(-v1)))
      }
      case _ => Failure(new RuntimeException("That didn't work"))
    }
  }
  def evalSigns(m: Store)(l: Expr, sign: String, r: Expr): Result = {
    val v1 = evaluate(m)(l)
    val v2 = evaluate(m)(r)
    (v1, v2) match {
      case (Success(Cell(Num(v1))), Success(Cell(Num(v2)))) =>
        Success(Cell(Num(signs(v1, sign, v2))))
      case _ => Failure(new RuntimeException("That didn't work"))
    }
  }
  def signs(v1: Int, sign: String, v2: Int) = sign match {
    case "+" => v1 + v2
    case "-" => v1 - v2
    case "*" => v1 * v2
    case "/" => v1 / v2
    case "%" => v1 % v2
  }

  def size(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
  }

  def height(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))
    case Var(v)      => 1
  }
  // Need Pretty Printer later. Only in block we'll worry about indentation. Try not to do it anywhere else
  // IMPORTANT ONE, HAVE TO COMPLETE
  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Constant(c)            => prefix + c.toString
    case UMinus(r)              => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r))
    case Plus(l, r)             => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Minus(l, r)            => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Times(l, r)            => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Div(l, r)              => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Mod(l, r)              => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Var(v)                 => prefix + v.toString
    case Loop(l, r)             => buildExprString(prefix, nodeString = "Loop", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Assignment(l, r)       => buildExprString(prefix, nodeString = "Assignment", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))

    case Block(b @ _*)          => buildBlockString(prefix, b)
    case Conditional(e, b1, b2) => buildTrinaryExprString(prefix, "Conditional", toFormattedString(prefix + INDENT)(e), toFormattedString(prefix + INDENT)(b1), toFormattedString(prefix + INDENT)(b2))
  }
  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def toPrettyFormatABC(prefix: String)(e: Expr): String = e match {

    case Constant(c)              => prefix + c.toString
    case UMinus(r)                => buildUnaryPrettyString(prefix, "-", toPrettyFormatABC(prefix)(r)) //TODO forgot what uminus does
    case Plus(l, r)               => buildPrettySign(prefix, " + ", toPrettyFormatABC(prefix)(l), toPrettyFormatABC(prefix)(r))
    case Minus(l, r)              => buildPrettySign(prefix, " - ", toPrettyFormatABC(prefix)(l), toPrettyFormatABC(prefix)(r))
    case Times(l, r)              => buildPrettySign(prefix, " * ", toPrettyFormatABC(prefix)(l), toPrettyFormatABC(prefix)(r))
    case Div(l, r)                => buildPrettySign(prefix, " / ", toPrettyFormatABC(prefix)(l), toPrettyFormatABC(prefix)(r))
    case Mod(l, r)                => buildPrettySign(prefix, " % ", toPrettyFormatABC(prefix)(l), toPrettyFormatABC(prefix)(r))
    case Var(v)                   => prefix + v.toString
    case Loop(l, r)               => buildPrettyLoop(prefix, "while", toPrettyFormatABC(prefix)(l), toPrettyFormatABC(prefix)(r))
    case Assignment(l, r)         => buildPrettyAssign(prefix, " = ", toPrettyFormatABC(prefix)(l), toPrettyFormatABC(prefix)(r))
    case Block(e @ _*)            => buildPrettyBlockString(prefix, e)
    case Conditional(con, b1, b2) => buildPrettyTrinary(prefix, toPrettyFormatABC(prefix)(con), toPrettyFormatABC(prefix)(b1), toPrettyFormatABC(prefix)(b2))

  }
  def toPrettyFormatABC(e: Expr): String = toPrettyFormatABC("")(e)

  def buildPrettyBlockString(prefix: String, e: Seq[Expr]) = {
    val result = new StringBuilder(prefix)
    result.append("{")
    result.append(EOL)
    val strings = e.map(expr => toPrettyFormatABC(prefix)(expr))
    strings.foreach(strings => result.append(strings))
    result.append(EOL)
    result.append("}")
    result.toString
  }

  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildTrinaryExprString(prefix: String, nodeString: String, conditional: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(conditional)
    result.append(",")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }
  def buildBlockString(prefix: String, nodeExprs: Seq[Expr]) = {
    val result = new StringBuilder(prefix)
    val strings: Seq[String] = nodeExprs.map(expr => toFormattedString(prefix)(expr))
    strings.foreach(string => result.append(string))
    result.toString

  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }
  def buildUnaryPrettyString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  def buildPrettySign(prefix: String, sign: String, leftExpr: String, rightExpr: String) = {
    val result = new StringBuilder(prefix)
    result.append("(")
    result.append(leftExpr)
    result.append(sign)
    result.append(rightExpr)
    result.append(")")
    result.toString
  }

  def buildPrettyAssign(prefix: String, sign: String, leftExpr: String, rightExpr: String) = {
    val result = new StringBuilder(prefix)
    result.append(leftExpr)
    result.append(sign)
    result.append(rightExpr)
    result.append(";")
    result.append(EOL)
    result.toString
  }

  def buildPrettyLoop(prefix: String, l: String, leftExpr: String, rightExpr: String) = {
    val result = new StringBuilder(prefix)
    result.append(l)
    result.append("(")
    result.append(leftExpr)
    result.append(")")
    result.append(rightExpr)
    result.toString
  }

  def buildPrettyTrinary(prefix: String, conditional: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append("if (")
    result.append(conditional)
    result.append(")")
    result.append(leftString)
    result.append(" else ")
    result.append(rightString)
    result.append(EOL)
    result.toString()
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}
