import edu.luc.cs.laufer.cs473.expressions.CombinatorParser

object example extends App {
  val res = CombinatorParser.parseAll(
    CombinatorParser.block,
    """
      |{
      |  i = 0;
      |  while(0) {
      |   if (1 * 2 - 3){
      |       x = 99;
      |   }
      |   else {
      |   }
      |  }
      |}
    """.stripMargin
  )

  val res1 = CombinatorParser.parseAll(CombinatorParser.topLevel, "x = 2;")
  val res2 = CombinatorParser.parseAll(CombinatorParser.topLevel, "if (1) { 1 + 2; }")

  println(res1)
  println(res2)
  println(res)

}
