import edu.luc.cs.laufer.cs473.expressions.behaviors.Value
import org.scalatest.FlatSpec
import edu.luc.cs.laufer.cs473.expressions.{CombinatorParser, behaviors, _}

import scala.collection.mutable.HashMap



class TestParsing extends FlatSpec {

  "A parser" should "evaluate a List" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "{}")
    assert(result.toString.contains("List()"))
  }

  "A parser" should "evaluate an assignment" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "i = 1;")
    assert(result.toString.contains("Block(List(Assignment(Var(List(i)),Constant(1))))"))
  }

  "A parser" should "evaluate multiple assignments" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "i = 1; t = 2; x = 3;")
    println(result.toString)
    assert(result.toString.contains("List("))
    assert(result.toString.contains("Assignment(Var(List(i)),Constant(1))"))
    assert(result.toString.contains("Assignment(Var(List(t)),Constant(2))"))
    assert(result.toString.contains("Assignment(Var(List(x)),Constant(3))"))
  }

  "A parser" should "evaluate an assignment to a var" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "t = 2; x = t;")
    println(result.toString)
    assert(result.toString.contains("Select("))
    assert(result.toString.contains("Assignment(Var(List(x))"))

  }

  "A parser" should "not evaluate an assignment without a semicolon" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "i = 1")
    assert(result.toString.contains("failure: ';' expected but end of source found"))
  }

  "A parser " should "evaluate a simple while loop" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "while(false){}")
    assert(result.toString.contains("Loop("))
  }
  "A parser" should "evaluate a conditional" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "if(true){}")
    assert(result.toString.contains("Conditional("))
  }

  "A parser" should " evaluate a struct" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "{i:10};")
    assert(result.toString.contains("Struct("))
    assert(result.toString.contains("(i,Constant(10))"))
  }

  "A parser " should "evaluate expressions with a hashmap store" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "1;")
    val store = HashMap.empty[String, Value]
    var res = behaviors.evaluate(store)(result.get)
    assert(res.toString.contains("Success("))
  }

  "A Parser" should "not evaluate expressions without a semicolon" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "4")
    val store = HashMap.empty[String, Value]
    assertThrows[java.lang.RuntimeException]{
      behaviors.evaluate(store)(result.get)
    }
  }

  "A Parser" should "evaluate simple mathematical expressions" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "4 + 3;")
    val store = HashMap.empty[String, Value]
    var res = behaviors.evaluate(store)(result.get)
    assert(res.toString.contains("Num(7)"))
  }

  "A Parser" should "recognize order of operations when evaluating" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "1 + 3 * 2 / 1;")
    val store = HashMap.empty[String, Value]
    var res = behaviors.evaluate(store)(result.get)
    assert(res.toString.contains("Num(7)"))
  }

}
