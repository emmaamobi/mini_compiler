package edu.luc.cs.laufer.cs473.expressions

import ast._
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

object behaviors {

  type Instance = HashMap[String, Value]
  type Store = Instance
  sealed trait Value
  case class Num(value: Int) extends Value
  type Result = Try[Value]

  def evaluate(m: Store)(e: Expr): Result = e match { //TODO for 3b
    case Constant(c) => Success(Num(c))
    case UMinus(r)   => evalUnary(m)(r, "-")
    case Plus(l, r)  => evalSigns(m)(l, "+", r)
    case Minus(l, r) => evalSigns(m)(l, "-", r)
    case Times(l, r) => evalSigns(m)(l, "*", r)
    case Div(l, r)   => evalSigns(m)(l, "/", r)
    case Mod(l, r)   => evalSigns(m)(l, "%", r)
    case Var(v) => {
      if (m.contains(v)) {
        Success(m(v))
      } else {
        Failure(new NoSuchFieldException(v))
      }
    }
    case Loop(l, r) => {
      while (evaluate(m)(l) != Success(Num(0))) {
        evaluate(m)(r)
      }
      evaluate(m)(l)
    }
    case Assignment(l, r) => {
      val valueL = l.toString.substring(l.toString.indexOf("(") + 1, l.toString.indexOf(")"))
      val valueRstr = r.toString.substring(r.toString.indexOf("(") + 1, r.toString.indexOf(")"))
      val valueR = evaluate(m)(r)
      valueR match {
        case Failure(thrown) => {
          Failure(new NoSuchFieldException(valueRstr))
        }
        case s => {
          if (m.contains(valueL)) {
            m(valueL) = s.get
            s
          } else {
            m += (valueL -> s.get)
            s
          }
        }
      }
    }
    case Block(s @ _*) => {
      val i = s.iterator
      var result: Value = null
      while (i.hasNext) {
        evaluate(m)(i.next()) match {
          case Success(r)     => result = r
          case f @ Failure(_) => return f
        }
      }
      Success(result)
    }
    case Conditional(e, l, r) => {
      val ans = evaluate(m)(e)
      ans match {
        case Failure(thrown) => {
          evaluate(m)(r)
        }
        case s => {
          evaluate(m)(l)
        }
      }
    }
  }
  def evalUnary(m: Store)(v: Expr, sign: String): Result = {
    val v1 = evaluate(m)(v)
    v1 match {
      case Success(Num(v1)) => Success(Num(evalUnarySign(m)(v1, sign)))
      case _                => Failure(new RuntimeException("That didn't work"))
    }
  }
  def evalUnarySign(m: Store)(v1: Int, sign: String): Int = sign match {
    case "-" => -v1
    case _   => v1
  }
  def evalSigns(m: Store)(l: Expr, sign: String, r: Expr): Result = {
    val v1 = evaluate(m)(l)
    val v2 = evaluate(m)(r)
    (v1, v2) match {
      case (Success(Num(v1)), Success(Num(v2))) => Success(Num(signs(v1, sign, v2)))
      case _                                    => Failure(new RuntimeException("That didn't work"))
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
