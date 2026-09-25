package slex

import scup.Pos

/** Thrown when no rule can match even one character: what and where. */
final case class LexError(message: String, pos: Pos) extends Exception(message):
  scup.SpecAssemble.diagnostic(this)  // the message says everything: keep only user frames

/**
 * One lexer rule: a regular expression plus what to do with the
 * lexeme it matches.  `action = None` means "match and discard"
 * (whitespace, comments).
 *
 * Build rules with the companion's helpers:
 *
 *   Rule(re"[0-9]+")((s, p) => Token(NUM, s, p))
 *   Rule.skip(re"[ \t\r\n]+")
 *
 * Actions receive the matched lexeme and the position of its first
 * character.  They run arbitrary code, which is where the
 * non-regular work of a scanner lives: keyword lookup, escape
 * processing, integer range checks.  An action may also throw --
 * e.g. your compiler's LexicalError for an out-of-range literal.
 *
 * `name` is how traces and dumps display the rule; spec files set it
 * to the pattern text you wrote, and "" means "render the regex".
 */
final case class Rule[T](re: Re, action: Option[(String, Pos) => T],
                         name: String = "")

object Rule:
  /** A rule that produces a token. */
  def apply[T](re: Re)(action: (String, Pos) => T): Rule[T] = Rule(re, Some(action))

  /** The same, with a display name for traces and dumps. */
  def apply[T](re: Re, name: String)(action: (String, Pos) => T): Rule[T] =
    Rule(re, Some(action), name)

  /** A rule whose matches are discarded (whitespace, comments). */
  def skip[T](re: Re, name: String = ""): Rule[T] = Rule(re, None, name)

/**
 * Which machine a scan runs.  `Dfa` (the default) is what lex would
 * generate: a table walk, one lookup per character.  `Nfa` is the
 * simulation that DFA was built from -- Lab 1's algorithm on HW 2's
 * construction -- kept so the same input can be watched through
 * both.  Same tokens, same errors, same trace.
 */
enum Engine:
  case Dfa, Nfa

/**
 * One munch's result: the longest match (`len` 0 means none), how
 * many characters the engine examined to find it, and whether the
 * machine was still alive when it stopped -- it ran out of input
 * rather than into a dead state.
 */
private[slex] final case class Munch(len: Int, rule: Int, examined: Int, alive: Boolean)

/**
 * The narrator both engines feed when a scan is traced: after each
 * character, which rules are still alive and which accept; then how
 * the munch ended.  One voice for both machines, so a trace reads
 * the same whichever engine produced it.
 */
private[slex] final class Narrator(trace: String => Unit, label: Int => String):
  def lbl(r: Int): String = s"#${r + 1}" + (if label == null then "" else s" ${label(r)}")

  /** After character c, the n-th of this munch.  `alive` and
   *  `accepts` are sorted rule indices. */
  def step(c: Char, n: Int, alive: List[Int], accepts: List[Int]): Unit =
    trace(s"  ${Nfa.showChar(c)}   alive: " +
      (if alive.isEmpty then "(no rule)" else alive.map(lbl).mkString(", ")))
    val bestNote = s"(best is now $n char${if n == 1 then "" else "s"})"
    accepts match
      case Nil      => ()
      case r :: Nil => trace(s"        accept: ${lbl(r)}  $bestNote")
      case rs       => trace(s"        accept: ${rs.map(lbl).mkString(", ")} -- " +
                             s"first listed wins: ${lbl(rs.head)}  $bestNote")

  /** How the munch ended.  `boundary` is the non-ASCII character
   *  that cut the input short, if the machine ran into one. */
  def finish(m: Munch, boundary: Option[Char]): Unit =
    if m.alive then boundary match
      case None    => trace("  (end of input)")
      case Some(c) => trace(s"  (non-ASCII character ${Nfa.showChar(c)} ends the munch)")
    if m.len > 0 then
      val backup = m.examined - m.len
      trace(s"  longest match: ${lbl(m.rule)} takes ${m.len} char${if m.len == 1 then "" else "s"}" +
        (if backup > 0 then s"  (backing up $backup)" else ""))
    else trace("  no rule matched even one character")

/**
 * A lexer is an ordered list of rules, exactly like the rule section
 * of a lex/flex specification.  Repeatedly, starting from the
 * current position:
 *
 *   1. find the LONGEST lexeme any rule can match (maximal munch);
 *   2. if several rules match that same longest lexeme, the rule
 *      listed FIRST wins;
 *   3. run the winner's action (or discard, for a skip rule) and
 *      continue after the lexeme.
 *
 * Those two tie-breaking principles are why `<=` lexes as one token
 * even though `<` also matches, and why a keyword rule listed before
 * the identifier rule claims `while` -- the same rules as lex, and
 * the same ones you applied by hand in HW 2.
 *
 * The rules compile once into a combined NFA (Thompson's
 * construction) and from it a DFA (subset construction); a scan
 * walks the DFA's table unless asked for `engine = Engine.Nfa`,
 * which simulates the NFA instead.  Input is ASCII: the first
 * character outside it ends the scan with a LexError at its
 * position, under either engine.
 *
 * Scanning is one token at a time: `scan` starts a Scan whose
 * next() produces a single token per call, so every token before a
 * bad character is produced before that character's error.  If no
 * rule matches even one character, next() throws a LexError at that
 * position (and actions may throw their own errors, e.g. your
 * compiler's LexicalError).  `tokenize` is the scan-everything
 * convenience built on top.
 *
 * Both take the same `trace` argument as scup's parse: `true`
 * narrates the scan to stdout (any `String => Unit` sink works in
 * its place), one munch at a time -- the alive rules after each
 * character, every accept and who wins its tie, the final munch and
 * back-up, and the token produced.  The two laws, watchable.
 */
final class Lexer[T](rulesSeq: Rule[T]*):
  private val rules = rulesSeq.toIndexedSeq
  private[slex] val nfa = Nfa.compile(rules.map(_.re))
  private[slex] lazy val dfa = Dfa.compile(nfa)

  /** Rule r, displayable: its spec text, or its rendered regex. */
  private[slex] def label(r: Int): String =
    if rules(r).name.nonEmpty then rules(r).name else rules(r).re.show

  private def ruleListing: String =
    val sb = StringBuilder()
    sb ++= "Rules (listing order breaks ties):\n"
    for i <- rules.indices do
      sb ++= f"  #${i + 1}%-3d ${label(i)}${if rules(i).action.isEmpty then "   (skip)" else ""}%n"
    sb.result()

  /** The rules and both machines, printable. */
  def dump(): String = ruleListing + "\n" + nfa.dump(label) + "\n" + dfa.dump(label)

  /** Start scanning `src` from its first character. */
  def scan(src: String, trace: scup.Trace = false, engine: Engine = Engine.Dfa): Scan =
    Scan(src, Lexer.sink(trace), engine)

  /**
   * An in-progress scan of one source string.  Each next() call
   * munches skip-rule matches and then returns Some(token), or None
   * at the end of the input.  It throws a LexError if no rule
   * matches even one character at the current position, or when it
   * reaches a non-ASCII character.
   */
  final class Scan(src: String, trace: String => Unit = null,
                   engine: Engine = Engine.Dfa):
    private var i    = 0
    private var line = 1
    private var col  = 1

    // Input is ASCII: the first character outside it bounds every munch.
    private val limit: Int =
      var j = 0
      while j < src.length && src.charAt(j) < Dfa.Alphabet do j += 1
      j
    private val boundary: Option[Char] =
      if limit < src.length then Some(src.charAt(limit)) else None

    private val narrator = if trace == null then null else Narrator(trace, label)

    if trace != null then
      trace(ruleListing + (engine match
        case Engine.Dfa => f"Engine: DFA (${dfa.size} states)%n"
        case Engine.Nfa => f"Engine: NFA (${nfa.size} states)%n"))

    /** The position of index j >= i, counting lines from the current one. */
    private def posAt(j: Int): Pos =
      var l = line; var c = col
      for k <- i until j do
        if src.charAt(k) == '\n' then { l += 1; c = 1 } else c += 1
      Pos(l, c)

    def next(): Option[T] =
      while i < src.length do
        val pos = Pos(line, col)
        if trace != null then
          val peek = src.substring(i, math.min(i + 12, src.length))
          trace(s"scan at $line:$col: ${Nfa.showString(peek)}" +
            (if i + 12 < src.length then "..." else ""))
        val m = engine match
          case Engine.Dfa => dfa.longestMatch(src, i, limit, narrator)
          case Engine.Nfa => nfa.longestMatch(src, i, limit, narrator)
        if trace != null then narrator.finish(m, boundary)
        if m.len == 0 then
          if m.alive && boundary.isDefined then   // it ran into the non-ASCII character
            throw LexError(s"non-ASCII character '${boundary.get}' -- slex reads ASCII input only",
                           posAt(limit))
          throw LexError(s"illegal character '${src.charAt(i)}'", pos)
        val lexeme = src.substring(i, i + m.len)
        for c <- lexeme do
          if c == '\n' then { line += 1; col = 1 }
          else col += 1
        i += m.len
        rules(m.rule).action match
          case Some(action) =>
            val t = action(lexeme, pos)
            if trace != null then { trace(s"  => $t"); trace("") }
            return Some(t)
          case None =>                     // skip rule: keep munching
            if trace != null then { trace("  => skipped"); trace("") }
      if trace != null then trace(s"scan at $line:$col: end of input")
      None

  /** Scan the whole source into tokens (skips discarded lexemes). */
  def tokenize(src: String, trace: scup.Trace = false,
               engine: Engine = Engine.Dfa): IndexedSeq[T] =
    val s    = scan(src, trace, engine)
    val toks = Vector.newBuilder[T]
    @annotation.tailrec
    def loop(): IndexedSeq[T] =
      s.next() match
        case Some(t) => toks += t; loop()
        case None    => toks.result()
    loop()

object Lexer:
  /** The line sink a trace argument denotes; null when tracing is off
   *  (the same convention as scup's parse). */
  private[slex] def sink(t: scup.Trace): String => Unit = t match
    case b: Boolean => if b then s => Console.println(s) else null
    case f          => f.asInstanceOf[String => Unit]
