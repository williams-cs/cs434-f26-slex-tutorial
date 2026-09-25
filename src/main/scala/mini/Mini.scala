// Mini.scala -- lexing one line with the mini lexer
package mini

import slex.{LexError, LexSpec}

enum K:
  case WHILE, IF, ELSE, LEQ, EQ, ASSIGN, LT, LP, RP, LB, RB, SEMI, ID, NUM

@main def demo(input: String*) =
  val tracing = input.headOption.contains("-trace")   // see Watching It Scan
  val rest = if tracing then input.tail else input
  val mini = LexSpec.load[K]("mini.slex")
  val line = if rest.isEmpty then "while (i <= 10) { i = i1; } // done"
             else rest.mkString(" ")
  try
    for t <- mini.tokenize(line, trace = tracing) do
      println(t)
  catch case e: LexError => println(s"lex error at ${e.pos}: ${e.message}")
