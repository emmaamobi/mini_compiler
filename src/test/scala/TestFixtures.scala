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
  val simple1string = "x = 5;"
  val simple1 = Block(
    Assignment(Var("x"), Constant(5))
  );

  //  val simple1ugly =
  //    """Assignment(
  //      ..x,
  //      ..2)
  //  """.stripMargin

  val simple2string = "x = 5; y = 7;"
  val simple2 = Block(
    Assignment(Var("x"), Constant(5)),
    Assignment(Var("y"), Constant(7))
  );

  val simple3string = "((1 + y2) - (3 * y4)) / 5;"
  val simple3 = Block(
    Div(
      Minus(
        Plus(
          Constant(1), Var("y2"),
        ), Times(
          Constant(3), Var("y4"),
        ),
      ), Constant(5),
    )
  )

  val simple4string = "if (1) { x = 2; } else { x = 3; }"
  val simple4 = Block(
    Conditional(
      Constant(1), Block(
        Assignment(Var("x"), Constant(2))
      ),
      Block(
        Assignment(Var("x"), Constant(3))
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

  val simple6string = "if (1) { x = 2; }"
  val simple6 = Block(
    Conditional(
      Constant(1), Block(
        Assignment(
          Var("x"), Constant(2),
        )
      ),
      Block(),
    )
  )

  val complex4string = "while (y) { if (3) { y = 3; } else { y = 1; }}"
  val complex4 = Block(
    Loop(
      Var("y"), Block(
        Conditional(Constant(3), Block(Assignment(Var("y"), Constant(3))), Block(Assignment(Var("y"), Constant(1))))
      )
    ),
  );

  val complex5string = "{ r = r + x; y = y + 1 ; }"
  val complex5 = Block(
    Block(
      Assignment(
        Var("r"), Plus(Var("r"), Var("x")),
      ), Assignment(
        Var("y"), Plus(Var("y"), Constant(1)),
      ),
    )
  )

  val complex6string = "if (4) { r = r + x; y = y + 1; }"
  val complex6 = Block(
    Conditional(
      Constant(4), Block(
        Assignment(
          Var("r"), Plus(
            Var("r"), Var("x")
          ),
        ), Assignment(
          Var("y"), Plus(
            Var("y"), Constant(1),
          ),
        ),
      ),
      Block(),
    )
  )

  val complex7string = "while (y) { r = r + x; y = y - 1; }"
  val complex7 = Block(
    Loop(
      Var("y"), Block(
        Assignment(
          Var("r"), Plus(Var("r"), Var("x")),
        ), Assignment(
          Var("y"), Minus(Var("y"), Constant(1)),
        ),
      ),
    )
  )

  val complex8string = "while (y) { r = r + x ; y = y - 1 ;}"
  val complex8 = Block(
    Loop(
      Var("y"), Block(
        Assignment(
          Var("r"), Plus(Var("r"), Var("x")),
        ), Assignment(
          Var("y"), Minus(Var("y"), Constant(1)),
        ),
      ),
    )
  )

}
