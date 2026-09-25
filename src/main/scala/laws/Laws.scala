// Laws.scala -- the two tie-breaking laws, on real input
package laws

import slex.{LexError, LexSpec}

enum RelK:
  case LT, LEQ

enum KwK:
  case WHILE, ID

def show[K](spec: slex.LexSpec[K], line: String, tracing: Boolean = false): Unit =
  try println(s"$line  ->  ${spec.tokenize(line, trace = tracing).mkString(" ")}")
  catch case e: LexError => println(s"$line  ->  lex error at ${e.pos}: ${e.message}")

/** Law 1: on `<=<`, LEQ matches two characters at the start, and
 *  length beats listing order. */
@main def rel(input: String*) =
  val tracing = input.headOption.contains("-trace")   // see Watching It Scan
  val rest = if tracing then input.tail else input
  val spec = LexSpec.load[RelK]("rel.slex")
  show(spec, if rest.isEmpty then "<=<" else rest.mkString(" "), tracing)

/** Law 2: on `while` the keyword row wins the tie; on `whilewhilst`
 *  the identifier rule matches MORE characters and Law 1 outranks
 *  Law 2. */
@main def kw(input: String*) =
  val tracing = input.headOption.contains("-trace")   // see Watching It Scan
  val rest = if tracing then input.tail else input
  val spec = LexSpec.load[KwK]("kw.slex")
  val lines = if rest.isEmpty then List("while", "whilewhilst")
              else List(rest.mkString(" "))
  for line <- lines do show(spec, line, tracing)
