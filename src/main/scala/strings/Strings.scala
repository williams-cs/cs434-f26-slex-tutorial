// Strings.scala -- what the blocks computed, not just the lexemes
package strings

import slex.{LexError, LexSpec}

enum K:
  case STR, NUM

@main def demo(input: String*) =
  val strs = LexSpec.load[K]("strings.slex")
  val line = if input.isEmpty then "\"hi\" 42" else input.mkString(" ")
  try
    for t <- strs.tokenize(line) do
      t.value match
        case () => println(t)                       // no computed value
        case v  => println(s"$t  (value = $v)")     // e.g. the parsed Int
  catch case e: LexError => println(s"lex error at ${e.pos}: ${e.message}")
