// Toy.scala -- load the spec and lex something
package toy

import slex.{LexError, LexSpec}

@main def demo(input: String*) =
  val toy = LexSpec.load[K]("toy.slex")
  val line = if input.isEmpty then "while (2 <= 10)" else input.mkString(" ")
  try
    for t <- toy.tokenize(line) do
      println(t)   // [WHILE,while,1], [LP,(,1], [NUM,2,1], ...
  catch case e: LexError => println(s"lex error at ${e.pos}: ${e.message}")
