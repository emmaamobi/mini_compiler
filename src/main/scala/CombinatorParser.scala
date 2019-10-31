package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._

object CombinatorParser extends JavaTokenParsers {

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~! opt(("+" | "-") ~ term) ^^ {
      case l ~ None          => l
      case l ~ Some("+" ~ r) => Plus(l, r)
      case l ~ Some("-" ~ r) => Minus(l, r)
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! opt(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ None          => l
      case l ~ Some("*" ~ r) => Times(l, r)
      case l ~ Some("/" ~ r) => Div(l, r)
      case l ~ Some("%" ~ r) => Mod(l, r)
    }

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | ident ^^ {case s => Var(s)}
  )
//statement   ::= expression ";" | assignment | conditional | loop | block
  def statement: Parser[Expr] = //TODO might need to add this to the stuff in behaviors
    expr ~ ";" ^^ {case e ~ _ => e}
    | assignment
    | conditional
    | loop
    | block
//assignment  ::= ident "=" expression ";"
  def assignment: Parser[Expr] = //TODO might need to add this to the stuff in behaviors
   ident ~ "=" ~ expr ~ ";" ^^ {case i ~ _ ~ e ~ _=> Assignment(Var(i),e)}

  //conditional ::= "if" "(" expression ")" block [ "else" block ]
  // def conditional: Parser[Expr] = 
  //   "if" !~ "(" ~ expr ~ ")" ~ block ^^ {case "if" ~ _ ~ e ~ _ ~ b => Loop(e,b)
  //   }
  // TODO def conditional: Parser[Expr] =


  //loop   ::= "while" "(" expression ")" block
  def loop: Parser[Expr] =
    "while" !~ "(" ~ expr ~ ")" ~ block ^^ {case "while" ~ _ ~ e ~ _ ~ b => Loop(e,b)
    }
  //block       ::= "{" statement* "}"
  // TODO def block: Parser[Expr] =
}
