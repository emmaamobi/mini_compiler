package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite
import TestFixtures.{simple1, simple1string, simple2, simple2string, _}
import edu.luc.cs.laufer.cs473.expressions.ast.Expr

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
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

  testParser("Given complex1", complex1string, complex1)
  testParser("Given complex2", complex1string2, complex2)
  testParser("Testing single assignment", simple1string, simple1)
  testParser("Testing multiple assignments", simple2string, simple2)
}
