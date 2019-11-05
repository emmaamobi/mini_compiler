package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._

object CombinatorParser extends JavaTokenParsers {

  /** expr ::= term { { "+" | "-" } term }* */
  // def expr: Parser[Expr] =
  //   term ~! opt(("+" | "-") ~ term) ^^ {
  //     case l ~ None          => l
  //     case l ~ Some("+" ~ r) => Plus(l, r)
  //     case l ~ Some("-" ~ r) => Minus(l, r)
  //   }
  // attempt with foldleft
  def expr: Parser[Expr] =
    term ~! rep(("+" | "-") ~ term) ^^ {
      case l ~ r => r.foldLeft(l) {
        case (a, "+" ~ t) => Plus(a, t)
        case (a, "-" ~ t) => Minus(a, t)
      }
      // case l ~ r => r(0)._1 match {
      //   case "+" => Plus(l, r.toSeq.foldleft(0)((a, b) => Plus(a._2, b._2)))
      //   case "-" => Minus(l, r.toSeq.foldleft(0)((a, b) => Minus(a._2, b._2)))
      // }
      //
    }
  // def expr: Parser[Expr] =
  //   term ~! rep(("+" | "-") ~ term) ^^ {
  //     case l ~ Nil => l
  //     case l ~ r => r(0)._1 match {
  //       case "+" => Plus(l, r.toSeq.foldleft(0)((a, b) => Plus(a._2, b._2)))
  //       case "-" => Minus(l, r.toSeq.foldleft(0)((a, b) => Minus(a._2, b._2)))
  //     }
  //     //
  //   }

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
    // | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | "(" ~> expr <~ ")"
    | ident ^^ { case s => Var(s) }
  )

  //statement   ::= expression ";" | assignment | conditional | loop | block
  def statement: Parser[Expr] = //TODO might need to add this to the stuff in behaviors
    expr ~ ";" ^^ { case e ~ _ => e } | assignment | conditional | loop | block

  //assignment  ::= ident "=" expression ";"
  def assignment: Parser[Expr] = //TODO might need to add this to the stuff in behaviors
    ident ~ "=" ~ expr ~ ";" ^^ { case i ~ _ ~ e ~ _ => Assignment(Var(i), e) }

  def loop: Parser[Expr] =
    "while" ~ "(" ~ expr ~ ")" ~ block ^^ {
      case "while" ~ _ ~ e ~ _ ~ b => Loop(e, b)
    }

  /*
  TODO FIGURE OUT THE BLOCK FORMAT I THINK IT LOOKS SOMETHING LIKE THIS
   */
  def block: Parser[Expr] =
    "{" ~ rep(statement) ~ "}" ^^ {
      case _ ~ statements ~ _ => Block(statements: _*)
    } //a code block has an expr between two curly braces?

  //conditional ::= "if" "(" expression ")" block [ "else" block ]
  def conditional: Parser[Expr] =
    "if" ~ "(" ~ expr ~ ")" ~ block ~ opt("else" ~ block) ^^ {
      case "if" ~ _ ~ e ~ _ ~ l ~ None             => Conditional(e, l, Block())
      case "if" ~ _ ~ e ~ _ ~ l ~ Some("else" ~ r) => Conditional(e, l, r)
    }

  //loop   ::= "while" "(" expression ")" block

  //block       ::= "{" statement* "}"
  // TODO def block: Parser[Expr] =
}
