// Hw2.scala -- HW 2, problem 2: the lexer you simulate by hand,
// runnable.  `sbt "runMain hw.lex -trace abaabbaba"` narrates the
// same steps and shows each getToken result.
package hw

import slex.{LexError, LexSpec}

enum K:
  case Tok1, Tok2, Tok3

@main def lex(input: String*) =
  val tracing = input.headOption.contains("-trace")
  val rest = if tracing then input.tail else input
  val spec = LexSpec.load[K]("hw2.slex")
  val line = if rest.isEmpty then "abaabbaba" else rest.mkString(" ")
  try println(spec.tokenize(line, trace = tracing).mkString(" "))
  catch case e: LexError => println(s"lex error at ${e.pos}: ${e.message}")
