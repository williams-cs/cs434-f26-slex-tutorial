package slex

/**
 * Regular expressions as an AST.
 *
 * A regular expression here is exactly the inductive definition from
 * class: the empty string, a single symbol, concatenation,
 * alternation, and Kleene star.  Everything else (`+`, `?`, character
 * classes, string literals) is sugar built from those five cases.
 *
 * A symbol case carries a *predicate* on characters rather than a
 * single character, so one `Sym` node can represent a whole character
 * class like [a-z]; the `desc` string is only used in error messages.
 *
 * Build these with the combinators below, or write lex-style string
 * syntax with the `re"..."` interpolator in ReSyntax.scala:
 *
 *   val ident  = (Re.alpha | Re.ch('_')) ~ (Re.alnum | Re.ch('_')).rep
 *   val ident2 = re"[a-zA-Z_][a-zA-Z0-9_]*"       // the same thing
 */
enum Re:
  case Eps                                          // matches the empty string
  case Sym(desc: String, pred: Char => Boolean)     // matches one character
  case Cat(l: Re, r: Re)                            // l followed by r
  case Alt(l: Re, r: Re)                            // l or r
  case Star(r: Re)                                  // zero or more r

  /** Concatenation: this followed by that. */
  def ~(that: Re): Re = Cat(this, that)

  /** Alternation: this or that. */
  def |(that: Re): Re = Alt(this, that)

  /** Zero or more repetitions (Kleene star). */
  def rep: Re = Star(this)

  /** One or more repetitions: `r r*`. */
  def rep1: Re = Cat(this, Star(this))

  /** Zero or one occurrence: `r | eps`. */
  def opt: Re = Alt(this, Eps)

  /**
   * The pattern, printable, lex-style: `[0-9][0-9]*`, `"while"`,
   * `(a|b)*`.  Runs of literal characters print as a quoted string.
   * Spec-file rules display as the pattern you wrote; this rendering
   * is for rules built with the combinators (traces, dumps).
   */
  def show: String =
    // a Sym that matches exactly one known character, per its desc
    def lit(r: Re): Option[Char] = r match
      case Sym(d, _) if d.length == 3 && d.head == '\'' => Some(d(1))
      case _ => None
    def atom(r: Re): String = r match          // parenthesize if compound
      case Alt(_, _) | Cat(_, _) => s"(${r.show})"
      case _                     => r.show
    def cats(r: Re): List[Re] = r match        // flatten, dropping str's seed
      case Cat(l, r) => cats(l) ++ cats(r)
      case Eps       => Nil
      case x         => List(x)
    this match
      case Eps       => "ε"
      case Sym(d, _) => d
      case Star(r)   => atom(r) + "*"
      case Alt(l, Eps) => atom(l) + "?"
      case Alt(l, r) => s"${l.show}|${r.show}"
      case Cat(_, _) =>
        def group(ps: List[Re]): List[String] = ps match
          case Nil => Nil
          case _ =>
            val (run, rest) = ps.span(lit(_).isDefined)
            if run.length >= 2 then ("\"" + run.flatMap(lit).mkString + "\"") :: group(rest)
            else atom(ps.head) :: group(ps.tail)
        group(cats(this)).mkString

object Re:
  /** The single character c. */
  def ch(c: Char): Re = Sym(s"'$c'", _ == c)

  /** Exactly the string s (concatenation of its characters). */
  def str(s: String): Re = s.foldLeft(Eps: Re)((r, c) => Cat(r, ch(c)))

  /** Any one character in the inclusive range lo-hi, e.g. range('a','z'). */
  def range(lo: Char, hi: Char): Re = Sym(s"[$lo-$hi]", c => lo <= c && c <= hi)

  /** Any one of the characters in s. */
  def anyOf(s: String): Re = Sym(s"[$s]", s.contains(_))

  /** Any one character except those in s. */
  def noneOf(s: String): Re = Sym(s"[^$s]", c => !s.contains(c))

  /** Any single character at all (like `.` in lex, but including newline). */
  val any: Re = Sym("any character", _ => true)

  /** Common classes. */
  val digit: Re = range('0', '9')
  val alpha: Re = Sym("letter", _.isLetter)
  val alnum: Re = Sym("letter or digit", _.isLetterOrDigit)
