A simple expression evaluator in Scala. 
Includes two types of parsers:

- [Scala parser combinators](http://www.scala-lang.org/api/current/scala-parser-combinators/#scala.util.parsing.combinator.Parsers)
- [parboiled2 parsing expression grammars](https://github.com/sirthias/parboiled2)

To run the tests:

      sbt test

To run either of the calculator examples:

      sbt run

To run either of the test examples:

      sbt test:run
