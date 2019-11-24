import edu.luc.cs.laufer.cs473.expressions.CombinatorParser

object example {
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
  val res2 = CombinatorParser.parseAll(CombinatorParser.topLevel, "while (y) { if (3) { y = 3; } else { y = 1; }}")

  println(res1)
  println(res2)
  println(res)

}
