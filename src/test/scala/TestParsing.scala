import org.scalatest.FlatSpec
import edu.luc.cs.laufer.cs473.expressions.CombinatorParser

class TestParsing extends FlatSpec {

  "A parser" should "evaluate a List" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "{}")
    assert(result.toString.contains("List()"))
  }

  "A parser" should "evaluate an assignment" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "i = 1;")
    assert(result.toString.contains("Block(List(Assignment(Var(List(i)),Constant(1))))"))
  }

  "A parser" should "not evaluate an assignment without a semicolon" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "i = 1")
    assert(result.toString.contains("failure: ';' expected but end of source found"))
  }

  "A parser " should "evaluate a simple while loop" in {
    var result = CombinatorParser.parseAll(CombinatorParser.topLevel, "while(false){}")
    assert(result.toString.contains("Loop("))
  }

}
