package edu.luc.cs.laufer.cs473.expressions
import org.jline.reader.LineReaderBuilder
import org.jline.terminal.TerminalBuilder
import scala.util.control.Breaks._
import scala.collection.mutable.HashMap

sealed trait Value
case class Num(v: Int) extends Value

object CombinatorCalculator extends App {
  val terminal = TerminalBuilder.terminal
  val reader = LineReaderBuilder.builder.terminal(terminal).build
  val prompt = ">> Enter infix expression: "
  val store = HashMap.empty[String, Int]
  // println(Value(5))
  // println(store)
  // println(Value(5).get)

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
    breakable {
      while (true) {
        try {
          var line = reader.readLine(prompt)
          while (line.slice(line.length - 2, line.length) != "\n\n") {
            // println("Line is: " + line)
            line += reader.readLine("| ") + "\n"
          }
          processExpr(line)

        } catch {
          case x: org.jline.reader.EndOfFileException     => break
          case y: org.jline.reader.UserInterruptException => break
        }
      }

    }

    // scala.io.Source.stdin.getLines foreach { line =>
    //   processExpr(line)
    //   print("Enter infix expression: ")
    // }
  }
}
