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

  println(res)

}
