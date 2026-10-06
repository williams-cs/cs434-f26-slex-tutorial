package slex

import scup.*

/**
 * Lex-style string syntax for regular expressions:
 *
 *   re"[a-zA-Z_][a-zA-Z0-9_]*"     identifiers
 *   re"//[^\n]*"                   line comments
 *   re"while|if|else"              keywords
 *
 * Supported: literal characters; `.` (any character except newline);
 * escapes `\n` `\t` `\r` and `\x` for any non-alphanumeric x (any
 * other letter or digit escape, such as `\f` or `\d`, is an error
 * rather than silently meaning the letter itself); postfix
 * `*` `+` `?`; alternation `|`; grouping `( )`; character classes
 * `[abc]`, `[a-z0-9]`, `[^...]` (negated).
 *
 * The syntax is parsed -- what else? -- with scup, over
 * character tokens classified into a small terminal alphabet (each
 * metacharacter is its own kind; everything else is Plain).  The
 * grammar is the one you would expect, one precedence level per rule:
 *
 *   alt   ::= cat ('|' cat)*
 *   cat   ::= post*
 *   post  ::= atom ('*' | '+' | '?')*
 *   atom  ::= '(' alt ')' | class | '.' | escape | plain character
 *
 * A malformed pattern throws IllegalArgumentException at lexer
 * construction time, carrying scup's expected-terminals message.
 */
object ReSyntax:

  private final case class CTok(c: Char, pos: Pos) extends Tokenish

  private def lexChars(s: String): IndexedSeq[CTok] =
    s.zipWithIndex.map((c, i) => CTok(c, Pos(1, i + 1)))

  /** The terminal alphabet: one kind per metacharacter, Plain for the rest. */
  private enum CK:
    case Pipe, Star, Plus, Quest, LPar, RPar, LBrack, RBrack, Esc, Dot,
         Caret, Dash, Plain

  import CK.*

  private def classify(c: Char): CK = c match
    case '|'  => Pipe
    case '*'  => Star
    case '+'  => Plus
    case '?'  => Quest
    case '('  => LPar
    case ')'  => RPar
    case '['  => LBrack
    case ']'  => RBrack
    case '\\' => Esc
    case '.'  => Dot
    case '^'  => Caret
    case '-'  => Dash
    case _    => Plain

  /** A letter or digit escape other than \n \t \r, at column col. */
  private final class BadEscape(c: Char, col: Int) extends Exception(
    s"unsupported escape '\\$c' at column $col (slex escapes are \\n \\t \\r, " +
    "and \\x for any non-alphanumeric x)")

  private def unescape(t: CTok): Char = t.c match
    case 'n' => '\n'
    case 't' => '\t'
    case 'r' => '\r'
    case c if c.isLetterOrDigit => throw BadEscape(c, t.pos.col - 1)
    case c   => c

  private object G extends Grammar[CTok, CK](t => classify(t.c), Map(
      Pipe -> "'|'", Star -> "'*'", Plus -> "'+'", Quest -> "'?'",
      LPar -> "'('", RPar -> "')'", LBrack -> "'['", RBrack -> "']'",
      Esc -> "'\\'", Dot -> "'.'", Caret -> "'^'", Dash -> "'-'",
      Plain -> "a character")):

    /** Backslash escapes: \n \t \r are control characters, \x is x
     *  itself for non-alphanumeric x, and any other escape is an error. */
    lazy val escape: Rule[Char] = rule(
      (!Esc, oneOf("an escaped character")(CK.values.toSeq*))
        --> { t => unescape(t) })

    // '^' and '-' are metacharacters only inside a class; outside
    // they are ordinary characters.
    lazy val plainChar: Rule[Char] = rule(
      oneOf("a character")(Plain, Caret, Dash) --> { t => t.c })

    // ----- character classes:  [a-z0-9_]  [^\n] -----------------------

    /** Inside [...] only ] and \ are special. */
    lazy val classChar: Rule[Char] = rule(
      escape
      | oneOf("a class character")(
        Pipe, Star, Plus, Quest, LPar, RPar, LBrack, Dot, Caret, Dash, Plain)
        --> { t => t.c })

    /**
     * One class item: a single character, or a range lo-hi.  The two
     * forms share their first symbol, so the range tail is factored
     * out; it is greedy because '-' can itself be a class member, so
     * '-' both begins the tail and can follow it.
     */
    lazy val classItem: Rule[(String, Char => Boolean)] = rule(
      (classChar, rangeTail) --> { (lo, hi) =>
        hi match
          case Some(h) => (s"$lo-$h", (c: Char) => lo <= c && c <= h)
          case None    => (lo.toString, (c: Char) => c == lo) })

    lazy val rangeTail: Rule[Option[Char]] = greedyRule(
      (!Dash, classChar) --> { hi => Some(hi) }
      | empty --> { None })

    /** A leading '^' negates; anywhere else it is a class member. */
    lazy val classNeg: Rule[Boolean] = greedyRule(
      Caret --> { _ => true }
      | empty --> { false })

    lazy val charClass: Rule[Re] = rule(
      (!LBrack, classNeg, classItem.+, !RBrack) --> { (neg, items) =>
        val desc    = items.map(_._1).mkString
        val inClass = (c: Char) => items.exists(_._2(c))
        if neg then Re.Sym(s"[^$desc]", c => !inClass(c))
        else Re.Sym(s"[$desc]", inClass) })

    // ----- the expression grammar --------------------------------------

    val dotRe: Re = Re.Sym(".", _ != '\n')     // lex's `.` excludes newline

    lazy val atom: Rule[Re] = rule(
      (!LPar, alt, !RPar) --> { r => r }
      | charClass
      | Dot --> { _ => dotRe }
      | escape --> { c => Re.ch(c) }
      | plainChar --> { c => Re.ch(c) })

    lazy val post: Rule[Re] = rule(
      (atom, postOp.*) --> { (a, ops) =>
        ops.foldLeft(a)((r, op) => op match
          case '*' => r.rep
          case '+' => r.rep1
          case _   => r.opt) })

    lazy val postOp: Rule[Char] = rule(
      oneOf("a postfix operator")(Star, Plus, Quest) --> { t => t.c })

    lazy val cat: Rule[Re] = rule(
      post.* --> { rs => rs.foldLeft(Re.Eps: Re)(Re.Cat(_, _)) })

    lazy val alt: Rule[Re] =
      leftAssoc(cat)(Pipe) { (l, _, r) => Re.Alt(l, r) }

  def parse(pattern: String): Re =
    try G.parse(G.alt, lexChars(pattern))
    catch case e: (ParseError | BadEscape) =>
      throw scup.SpecAssemble.diagnostic(
        IllegalArgumentException(s"bad regex \"$pattern\": ${e.getMessage}"))

/**
 * The re"..." interpolator.  Escape sequences reach the parser
 * unprocessed (like raw"..."), so re"[^\n]*" means what it says.
 */
extension (sc: StringContext)
  def re(args: Any*): Re =
    require(args.isEmpty, "re\"...\" patterns cannot interpolate values")
    ReSyntax.parse(sc.parts.head)
