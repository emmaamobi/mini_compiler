package edu.luc.cs.laufer.cs473.expressions

import ast._

object behaviors {

  def evaluate(e: Expr): Int = e match {
    case Constant(c) => c
    case UMinus(r)   => -evaluate(r)
    case Plus(l, r)  => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Times(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r)   => evaluate(l) / evaluate(r)
    case Mod(l, r)   => evaluate(l) % evaluate(r)

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
    //TODO finish this
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

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}
