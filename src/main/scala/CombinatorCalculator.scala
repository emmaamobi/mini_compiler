package edu.luc.cs.laufer.cs473.expressions
import org.jline.reader.LineReaderBuilder
import org.jline.terminal.TerminalBuilder

object CombinatorCalculator extends App {
  val terminal = TerminalBuilder.terminal
  val reader = LineReaderBuilder.builder.terminal(terminal).build
  val prompt = "Enter infix expression: "

  // val store = behaviors.newstore
  def processExpr(input: String): Unit = {

    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.topLevel, input)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      import behaviors._
      val expr = result.get
      println("The parsed expression is: ")
      println(expr)
      println(toFormattedString(expr))
      println("The pretty form is:")
      println(toPrettyFormatABC(expr))
      // println("It has size " + size(expr) + " and height " + height(expr))
      // println("It evaluates to " + evaluate(expr))
    }
  }

  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    while (true) {
      processExpr(reader.readLine(prompt))
    }

    // scala.io.Source.stdin.getLines foreach { line =>
    //   processExpr(line)
    //   print("Enter infix expression: ")
    // }
  }
}
