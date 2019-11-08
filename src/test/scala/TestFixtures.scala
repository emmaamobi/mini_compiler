package edu.luc.cs.laufer.cs473.expressions

object TestFixtures {

  import ast._
  import behaviors._

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

  val simple1ugly =
    """Assignment(
  ..x,
  ..2)""".stripMargin

  val simple2string = "x = 3; y = 9;"

  val simple2 = Block(
    Assignment(Var("x"), Constant(3)),
    Assignment(Var("y"), Constant(9))
  );

  val simple3string = "if (1) { x = 5; }"

  val simple3 = Block(
    Conditional(
      Constant(1), Block(
        Assignment(Var("x"), Constant(5))
      ),
      Block(),
    )
  );

  val simple4string = "if (1) { x = 7; } else { x = 6; }"

  val simple4 = Block(
    Conditional(
      Constant(1), Block(
        Assignment(Var("x"), Constant(7))
      ),
      Block(
        Assignment(Var("x"), Constant(6))
      ),
    )
  );

  val complex3string = "{ x = 1 + 2; y = x + 2;}"

  val complex3 = Block(
    Block(
      Assignment(Var("x"), Plus(
        Constant(1), Constant(2)
      )),
      Assignment(Var("y"), Plus(
        Var("x"), Constant(2)
      ))
    )
  );

  val simple5string = "while (4) { 2 + 3;}"

  val simple5 = Block(
    Loop(
      Constant(4), Block(
        Plus(Constant(2), Constant(3))
      )
    ),
  )
  val complex4string = "while (y) { if (3) { y = 3; } else { y = 1; }}"

  val complex4 = Block(
    Loop(
      Var("y"), Block(
        Conditional(Constant(3), Block(Assignment(Var("y"), Constant(3))), Block(Assignment(Var("y"), Constant(1))))
      )
    ),
  );
}
