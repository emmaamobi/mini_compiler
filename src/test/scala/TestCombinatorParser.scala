package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite
import TestFixtures.{simple1, simple1string, simple2, simple2string, _}
import edu.luc.cs.laufer.cs473.expressions.ast.Expr
import behaviors._

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  //println(behaviors.evaluate(parsedExpr.get))
}

class TestCombinatorParser extends FunSuite {
  //  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  //  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string2)
  //  test("parser works 1") { assert(parsedExpr.get === complex1) }
  //  test("parser works 2") { assert(parsedExpr2.get === complex1) }
  def testParser(description: String, input: String, parsed: Expr): Unit = {
    test(description) {
      val parsedExpr = CombinatorParser.parseAll(CombinatorParser.topLevel, input)
      assert(parsedExpr.get === parsed)
    }
  }

  def testUglyPrinter(description: String, input: String, parsed: String): Unit = {
    test(description) {
      val parsedExpr = CombinatorParser.parseAll(CombinatorParser.topLevel, input).get
      val expr = toFormattedString(parsedExpr)
      assert(expr === parsed)
    }
  }

  //  def testPrettyPrinter(description: String, input: String, parsed: String): Unit = {
  //    test(description) {
  //      val parsedExpr = CombinatorParser.parseAll(CombinatorParser.topLevel, input).get
  //      val expr = toPrettyFormat(parsedExpr)
  //      assert(expr === parsed)
  //    }
  //  }

  //testUglyPrinter("Given complex1", complex1string, complex1)
  //testUglyPrinter("Given complex2", complex1string2, complex2)
  testParser("Test single assignment", simple1string, simple1)
  testParser("Test multiple assignments", simple2string, simple2)
  testParser("Test multiple operators", simple3string, simple3)
  testParser("Test simple conditional with else", simple4string, simple4)
  testParser("Test loop", simple5string, simple5)
  testParser("Test simple conditional", simple6string, simple6)
  testParser("Test multiple operators and assignments", complex3string, complex3)
  testParser("Test loop with conditional, and assignment", complex4string, complex4)
  testParser("Test variable assignments and operators", complex5string, complex5)
  testParser("Test conditionals, assignment, operators", complex6string, complex6)
  testParser("Test loop, assignment, operators", complex7string, complex7)
  testParser("Test loop, assignment, operators, with whitespace", complex8string, complex8)
  //testUglyPrinter("Testing ugly printer", simple1string, simple1ugly)
}
