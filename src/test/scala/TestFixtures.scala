package edu.luc.cs.laufer.cs473.expressions

object TestFixtures {

  import ast._

  val complex1 =
    Div(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          Constant(3),
          Constant(4)
        )
      ),
      Constant(5)
    );

  val complex1string = "((1 + 2) - (3 * 4)) / 5"

  val complex1string2 = "  ((1 + 2) - (3 * 4)) / 5  "

  val complex2 =
    Mod(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          UMinus(
            Constant(3)
          ),
          Constant(4)
        )
      ),
      Constant(5)
    );
  val simple1string = "x = 2;"

  val simple1 = Block(
    Assignment(Var("x"), Constant(2))
  );

  val simple2string = "x = 3; y = 9;"

  val simple2 = Block(
    Assignment(Var("x"), Constant(3)),
    Assignment(Var("y"), Constant(9))
  );

  val simple3string = "if (1) { 1 + 2; }"
}
