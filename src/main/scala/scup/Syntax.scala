package scup

/**
 * Thrown when a grammar is not LL(1): a FIRST/FIRST or FIRST/FOLLOW
 * conflict, left recursion, or a repeated symbol that can match the
 * empty string.  The message names the rules and terminals involved.
 */
final class GrammarError(msg: String) extends Exception(msg):
  SpecAssemble.diagnostic(this)  // the message says everything: keep only user frames

/**
 * Thrown when the input does not match the grammar.  The message
 * reads "expected X, Y or Z at line 1, column 7 (while parsing 'expr')".
 */
final case class ParseError(expected: List[String], pos: Pos, rule: String)
    extends Exception(
      s"expected ${expected.distinct.mkString(" or ")} at $pos (while parsing '$rule')"):
  SpecAssemble.diagnostic(this)  // the message says everything: keep only user frames

/**
 * An LL(1) grammar over tokens T, classified into a finite terminal
 * alphabet K by `kindOf`.  Subclass it and declare one `Rule` per
 * nonterminal, yacc-style: each rule is a list of *productions*, each
 * production a flat sequence of symbols with one action:
 *
 *   object Calc extends Grammar[Tok, Kind](_.kind):
 *     lazy val expr: Rule[Int] =
 *       leftAssoc(term)(PLUS, MINUS) { (l, op, r) => ... }
 *     lazy val factor: Rule[Int] = rule(
 *         NUM                      --> { n => n.text.toInt }
 *       | (!LPAREN, expr, !RPAREN) --> { e => e })
 *
 * A symbol is a bare terminal kind (its action value is the token
 * itself) or another rule (its action value is whatever that rule's
 * actions build).  A symbol prefixed with `~` (or wrapped in
 * `skip(...)`) still must match, but contributes no action argument
 * -- so actions name exactly the values that matter, like yacc
 * actions that mention only some of their `$i`.  `empty` is yacc's
 * %empty.  The EBNF modifiers `.?`, `.*`, `.+`, and `.*(sep)`
 * replace yacc's hand-written list nonterminals -- LL(1) forbids the
 * left-recursive list idiom anyway -- and match the notation of the
 * grammars in the course handouts.
 *
 * Rules take their names from the vals that hold them (rule name
 * reflection happens when the grammar is first used); pass a string
 * as the first argument of `rule` to override.
 *
 * The grammar is checked when it is first used (`parse`, `check`, or
 * `dump`): FIRST/FOLLOW sets are computed and any LL(1) conflict or
 * left recursion is rejected with a GrammarError.  There is no
 * backtracking: parsing is predictive, driven by one token of
 * lookahead, and error messages state exactly which terminals could
 * have appeared.
 */
abstract class Grammar[T <: Tokenish, K](
    val kindOf: T => K,
    kindNames: Map[K, String] = Map.empty[K, String])
    extends GrammarAnalysis[T, K], GrammarEngine[T, K]:

  /**
   * How a terminal kind is shown in error messages and dumps: the
   * `kindNames` entry if present (yacc's %token declarations), the
   * kind's own toString otherwise.  Override for full control.
   */
  def showKind(k: K): String = kindNames.getOrElse(k, k.toString)

  /*----------------------------- symbols -----------------------------*/

  /** A grammar symbol producing a value of type A. */
  sealed trait Sym[+A]

  /** A terminal: matches one token of the given kind, producing it. */
  final case class Term(kind: K) extends Sym[T]:
    override def toString = showKind(kind)

  /**
   * A nonterminal: a named list of productions.  Productions are
   * evaluated lazily (on first use) so mutually recursive rules can
   * reference each other freely as lazy vals.
   */
  final class Rule[+A] private[scup] (
      private[scup] var nameFn: () => String,
      private[scup] var named: Boolean,
      prodsThunk: () => Seq[Prod[A]],
      private[scup] val greedy: Boolean,
      private[scup] val loop: LoopKind = LoopKind.None,
  ) extends Sym[A]:
    private[scup] lazy val prods: IndexedSeq[Prod[A]] =
      prodsThunk().toIndexedSeq

    /** The rule's name: from its val, unless given explicitly. */
    def name: String = nameFn()
    override def toString = name

    /** Parse a complete token sequence with this rule as the start
     *  symbol.  Pass `trace = true` to watch every step. */
    def parse(toks: IndexedSeq[T], trace: Trace = false): A =
      Grammar.this.parse(this, toks, trace)

    /** Check that the grammar reachable from this rule is LL(1). */
    def check(): Unit = analysis(this)

    /** FIRST/FOLLOW sets and the LL(1) table, as a printable string. */
    def dump(): String = analysis(this).dump

  /** Marks synthesized list rules the engine runs as loops, not recursion. */
  private[scup] enum LoopKind:
    case None, Star, SepTail

  /** One production: a flat symbol sequence and its action. */
  final class Prod[+A] private[Grammar] (
      private[scup] val syms: List[Sym[?]],
      private[scup] val act: IndexedSeq[Any] => A)

  /*--------------------------- rule naming ---------------------------*/

  // Rules declared without an explicit name are named after the vals
  // that hold them, found by reflection over this grammar object the
  // first time the grammar is used.  (A macro cannot do this here:
  // the library is vendored as source, and Scala 3 macros cannot be
  // defined and used in the same compilation run.)
  private var baptized = false
  private[scup] def baptize(): Unit =
    if !baptized then
      baptized = true
      for m <- getClass.getMethods
          if m.getParameterCount == 0
             && classOf[Rule[?]].isAssignableFrom(m.getReturnType)
             && !m.getName.contains("$") do
        try
          m.setAccessible(true)   // grammar objects are often private
          m.invoke(this) match
            case r: Rule[?] @unchecked =>
              if !r.named then
                val n = m.getName
                r.nameFn = () => n
                r.named = true
            case _ => ()
        catch case _: Exception => ()

  /*------------------------ symbol evidence --------------------------*/

  /**
   * Evidence that X can appear where a *value-producing* symbol is
   * needed (modifier receivers, separators, operands): either a bare
   * terminal kind K or a Sym.  Type-level plumbing; grammar authors
   * never see it.
   */
  sealed trait AsSym[-X, A]:
    def sym(x: X): Sym[A]

  given kindIsSym: AsSym[K, T] with
    def sym(k: K): Sym[T] = Term(k)
  given symIsSym[A]: AsSym[Sym[A], A] with
    def sym(s: Sym[A]): Sym[A] = s

  /**
   * A symbol marked to match but bind nothing: yacc punctuation.
   * Written `!SEMI` (or, equivalently, `skip(SEMI)`).
   */
  final class Skip private[Grammar] (private[Grammar] val sym: Sym[?])

  def skip[X, A](x: X)(using s: AsSym[X, A]): Skip = new Skip(s.sym(x))

  extension (k: K) def unary_! : Skip = new Skip(Term(k))
  extension (s: Sym[?]) @annotation.targetName("skipSym") def unary_! : Skip = new Skip(s)

  /**
   * Evidence that X is one element of a production: a value-carrying
   * symbol contributing one action argument (Out = Tuple1[...]) or a
   * Skip contributing none (Out = EmptyTuple).
   */
  sealed trait Arg[-X]:
    type Out <: Tuple
    def info(x: X): (Sym[?], Boolean)   // (symbol, contributes a value)

  type ArgAux[X, O <: Tuple] = Arg[X] { type Out = O }

  given skipIsArg: ArgAux[Skip, EmptyTuple] = new Arg[Skip]:
    type Out = EmptyTuple
    def info(x: Skip) = (x.sym, false)
  given kindIsArg: ArgAux[K, Tuple1[T]] = new Arg[K]:
    type Out = Tuple1[T]
    def info(k: K) = (Term(k), true)
  given symIsArg[A]: ArgAux[Sym[A], Tuple1[A]] = new Arg[Sym[A]]:
    type Out = Tuple1[A]
    def info(s: Sym[A]) = (s, true)

  /**
   * The action type for a production whose value-carrying symbols
   * have types Args: one parameter per kept symbol (a bare A => R
   * for exactly one; Unit => R when everything is skipped).
   */
  type FnFrom[Args <: Tuple, R] = Args match
    case EmptyTuple      => Unit => R
    case a *: EmptyTuple => a => R
    case _               => Args => R

  /*------------------------ building productions ---------------------*/

  private[scup] def mkProd[A](syms: List[Sym[?]], act: IndexedSeq[Any] => A): Prod[A] =
    new Prod(syms, act)

  private def prodOf[R](infos: List[(Sym[?], Boolean)], f: Any): Prod[R] =
    val keeps = infos.map(_._2)
    val nKept = keeps.count(identity)
    mkProd(infos.map(_._1), vs => {
      val kept = new Array[Any](nKept)
      var i = 0; var j = 0
      for keep <- keeps do
        if keep then { kept(j) = vs(i); j += 1 }
        i += 1
      nKept match
        case 0 => f.asInstanceOf[Unit => R](())
        case 1 => f.asInstanceOf[Any => R](kept(0))
        case _ => f.asInstanceOf[Tuple => R](Tuple.fromArray(kept))
    })

  /** yacc's %empty: a production matching nothing. */
  object empty:
    def -->[R](f: => R): Prod[R] = mkProd(Nil, _ => f)

  extension [X1, O1 <: Tuple](x: X1)(using a1: ArgAux[X1, O1])
    def -->[R](f: FnFrom[O1, R]): Prod[R] =
      prodOf(List(a1.info(x)), f)

    /**
     * Zero or one occurrence: the grammar figures' `[x]`.
     * (`.?`, `.*`, `.+` mirror the EBNF of the course handouts and
     * the names of the synthesized rules in `dump()`.)
     */
    def ?[A](using s1: AsSym[X1, A]): Sym[Option[A]] = optRule(s1.sym(x))

    /**
     * Zero or more occurrences (right-recursive internally).
     * Repetition is *greedy*: when a terminal could both continue
     * the repetition and follow it, the repetition continues -- the
     * standard LL resolution, and the one that binds trailing
     * operators to the innermost construct.  (A terminal that
     * conflicts with the repeated symbol's own FIRST set is still an
     * error.)
     */
    def *[A](using s1: AsSym[X1, A]): Sym[List[A]] = starRule(s1.sym(x))

    /** One or more occurrences. */
    def +[A](using s1: AsSym[X1, A]): Sym[List[A]] = rep1Rule(s1.sym(x))

    /** Zero or more occurrences separated by `sep` (whose value is dropped). */
    def *[A, Y, B](sep: Y)(using s1: AsSym[X1, A], ss: AsSym[Y, B]): Sym[List[A]] =
      repSepRule(s1.sym(x), ss.sym(sep))

  extension [X1, O1 <: Tuple, X2, O2 <: Tuple](t: (X1, X2))(
      using a1: ArgAux[X1, O1], a2: ArgAux[X2, O2])
    def -->[R](f: FnFrom[Tuple.Concat[O1, O2], R]): Prod[R] =
      prodOf(List(a1.info(t._1), a2.info(t._2)), f)

  extension [X1, O1 <: Tuple, X2, O2 <: Tuple, X3, O3 <: Tuple](t: (X1, X2, X3))(
      using a1: ArgAux[X1, O1], a2: ArgAux[X2, O2], a3: ArgAux[X3, O3])
    def -->[R](f: FnFrom[Tuple.Concat[O1, Tuple.Concat[O2, O3]], R]): Prod[R] =
      prodOf(List(a1.info(t._1), a2.info(t._2), a3.info(t._3)), f)

  extension [X1, O1 <: Tuple, X2, O2 <: Tuple, X3, O3 <: Tuple, X4, O4 <: Tuple](
      t: (X1, X2, X3, X4))(
      using a1: ArgAux[X1, O1], a2: ArgAux[X2, O2], a3: ArgAux[X3, O3],
            a4: ArgAux[X4, O4])
    def -->[R](f: FnFrom[Tuple.Concat[O1, Tuple.Concat[O2, Tuple.Concat[O3, O4]]], R]): Prod[R] =
      prodOf(List(a1.info(t._1), a2.info(t._2), a3.info(t._3), a4.info(t._4)), f)

  extension [X1, O1 <: Tuple, X2, O2 <: Tuple, X3, O3 <: Tuple, X4, O4 <: Tuple,
             X5, O5 <: Tuple](t: (X1, X2, X3, X4, X5))(
      using a1: ArgAux[X1, O1], a2: ArgAux[X2, O2], a3: ArgAux[X3, O3],
            a4: ArgAux[X4, O4], a5: ArgAux[X5, O5])
    def -->[R](f: FnFrom[Tuple.Concat[O1, Tuple.Concat[O2, Tuple.Concat[O3,
        Tuple.Concat[O4, O5]]]], R]): Prod[R] =
      prodOf(List(a1.info(t._1), a2.info(t._2), a3.info(t._3), a4.info(t._4),
                  a5.info(t._5)), f)

  extension [X1, O1 <: Tuple, X2, O2 <: Tuple, X3, O3 <: Tuple, X4, O4 <: Tuple,
             X5, O5 <: Tuple, X6, O6 <: Tuple](t: (X1, X2, X3, X4, X5, X6))(
      using a1: ArgAux[X1, O1], a2: ArgAux[X2, O2], a3: ArgAux[X3, O3],
            a4: ArgAux[X4, O4], a5: ArgAux[X5, O5], a6: ArgAux[X6, O6])
    def -->[R](f: FnFrom[Tuple.Concat[O1, Tuple.Concat[O2, Tuple.Concat[O3,
        Tuple.Concat[O4, Tuple.Concat[O5, O6]]]]], R]): Prod[R] =
      prodOf(List(a1.info(t._1), a2.info(t._2), a3.info(t._3), a4.info(t._4),
                  a5.info(t._5), a6.info(t._6)), f)

  extension [X1, O1 <: Tuple, X2, O2 <: Tuple, X3, O3 <: Tuple, X4, O4 <: Tuple,
             X5, O5 <: Tuple, X6, O6 <: Tuple, X7, O7 <: Tuple](
      t: (X1, X2, X3, X4, X5, X6, X7))(
      using a1: ArgAux[X1, O1], a2: ArgAux[X2, O2], a3: ArgAux[X3, O3],
            a4: ArgAux[X4, O4], a5: ArgAux[X5, O5], a6: ArgAux[X6, O6],
            a7: ArgAux[X7, O7])
    def -->[R](f: FnFrom[Tuple.Concat[O1, Tuple.Concat[O2, Tuple.Concat[O3,
        Tuple.Concat[O4, Tuple.Concat[O5, Tuple.Concat[O6, O7]]]]]], R]): Prod[R] =
      prodOf(List(a1.info(t._1), a2.info(t._2), a3.info(t._3), a4.info(t._4),
                  a5.info(t._5), a6.info(t._6), a7.info(t._7)), f)

  extension [X1, O1 <: Tuple, X2, O2 <: Tuple, X3, O3 <: Tuple, X4, O4 <: Tuple,
             X5, O5 <: Tuple, X6, O6 <: Tuple, X7, O7 <: Tuple, X8, O8 <: Tuple](
      t: (X1, X2, X3, X4, X5, X6, X7, X8))(
      using a1: ArgAux[X1, O1], a2: ArgAux[X2, O2], a3: ArgAux[X3, O3],
            a4: ArgAux[X4, O4], a5: ArgAux[X5, O5], a6: ArgAux[X6, O6],
            a7: ArgAux[X7, O7], a8: ArgAux[X8, O8])
    def -->[R](f: FnFrom[Tuple.Concat[O1, Tuple.Concat[O2, Tuple.Concat[O3,
        Tuple.Concat[O4, Tuple.Concat[O5, Tuple.Concat[O6,
        Tuple.Concat[O7, O8]]]]]]], R]): Prod[R] =
      prodOf(List(a1.info(t._1), a2.info(t._2), a3.info(t._3), a4.info(t._4),
                  a5.info(t._5), a6.info(t._6), a7.info(t._7), a8.info(t._8)), f)

  extension [X1, O1 <: Tuple, X2, O2 <: Tuple, X3, O3 <: Tuple, X4, O4 <: Tuple,
             X5, O5 <: Tuple, X6, O6 <: Tuple, X7, O7 <: Tuple, X8, O8 <: Tuple,
             X9, O9 <: Tuple](t: (X1, X2, X3, X4, X5, X6, X7, X8, X9))(
      using a1: ArgAux[X1, O1], a2: ArgAux[X2, O2], a3: ArgAux[X3, O3],
            a4: ArgAux[X4, O4], a5: ArgAux[X5, O5], a6: ArgAux[X6, O6],
            a7: ArgAux[X7, O7], a8: ArgAux[X8, O8], a9: ArgAux[X9, O9])
    def -->[R](f: FnFrom[Tuple.Concat[O1, Tuple.Concat[O2, Tuple.Concat[O3,
        Tuple.Concat[O4, Tuple.Concat[O5, Tuple.Concat[O6, Tuple.Concat[O7,
        Tuple.Concat[O8, O9]]]]]]]], R]): Prod[R] =
      prodOf(List(a1.info(t._1), a2.info(t._2), a3.info(t._3), a4.info(t._4),
                  a5.info(t._5), a6.info(t._6), a7.info(t._7), a8.info(t._8),
                  a9.info(t._9)), f)

  /*----------------------------- rules -------------------------------*/

  /**
   * An alternative in a rule: a production, or a bare symbol used as
   * a pass-through production (its value is the rule's value).
   */
  type Alt[A] = Prod[A] | Sym[A]

  /** A `|`-chain of alternatives, as they appear inside `rule(...)`. */
  final class Alts[+A] private[Grammar] (
      private[Grammar] val list: List[Prod[A]])

  private def altProd[A](x: Alt[A]): Prod[A] = x match
    case p: Prod[?] => p.asInstanceOf[Prod[A]]
    case s: Sym[?]  => mkProd(List(s.asInstanceOf[Sym[A]]), vs => vs(0).asInstanceOf[A])

  // yacc's `|` between productions.  (`-->` starts with '-', which
  // binds tighter than '|' in Scala, so `a --> f | b --> g` groups
  // exactly as yacc reads it.)
  extension [A](p: Prod[A])
    def |[B >: A](q: Alt[B]): Alts[B] = new Alts(List(p, altProd(q)))
  extension [A](s: Sym[A])
    @annotation.targetName("orSym")
    def |[B >: A](q: Alt[B]): Alts[B] = new Alts(List(altProd(s), altProd(q)))
  extension [A](as: Alts[A])
    @annotation.targetName("orAlts")
    def |[B >: A](q: Alt[B]): Alts[B] = new Alts(as.list.map(p => p: Prod[B]) :+ altProd(q))

  private[scup] def mkRule[A](name: String | Null, greedy: Boolean, loop: LoopKind,
                        ps: () => Seq[Prod[A]]): Rule[A] =
    if name == null then new Rule(() => "?", false, ps, greedy, loop)
    else new Rule(() => name, true, ps, greedy, loop)

  // The whole `|`-chain is one by-name argument, evaluated on first
  // use of the grammar, so mutually recursive lazy-val rules never
  // force each other during initialization.
  private def mkRuleAlts[A](name: String | Null, greedy: Boolean,
                            alts: => (Alts[A] | Alt[A])): Rule[A] =
    mkRule(name, greedy, LoopKind.None, () => alts match
      case as: Alts[A] @unchecked => as.list
      case a => List(altProd(a.asInstanceOf[Alt[A]])))

  /**
   * Define a nonterminal from a `|`-chain of alternatives:
   *
   *   lazy val stmt: Rule[StmtNode] = rule(
   *       block
   *     | (IF, !LPAREN, expr, !RPAREN, stmt, elseTail) --> { ... }
   *     | (BREAK, !SEMI) --> { b => BreakNode(b.line) })
   *
   * The chain is by-name, so mutually recursive lazy-val rules never
   * force each other during initialization.  The rule is named after
   * the val that holds it; pass a string first to override.
   */
  def rule[A](alts: => (Alts[A] | Alt[A])): Rule[A] =
    mkRuleAlts(null, false, alts)
  def rule[A](name: String)(alts: => (Alts[A] | Alt[A])): Rule[A] =
    mkRuleAlts(name, false, alts)

  /**
   * Like `rule`, but a FIRST/FOLLOW conflict is resolved in favor of
   * the production that consumes input, instead of being an error.
   * This is the standard resolution for the dangling else -- and that
   * is nearly the only legitimate use.
   */
  def greedyRule[A](alts: => (Alts[A] | Alt[A])): Rule[A] =
    mkRuleAlts(null, true, alts)
  def greedyRule[A](name: String)(alts: => (Alts[A] | Alt[A])): Rule[A] =
    mkRuleAlts(name, true, alts)

  /**
   * A rule matching any one of the given terminal kinds, producing
   * the token.  Chiefly for character-level grammars, where "a digit"
   * is ten one-terminal productions.
   */
  def oneOf(name: String)(ks: K*): Rule[T] =
    mkRule(name, false, LoopKind.None,
      () => ks.map(k => mkProd(List(Term(k)), vs => vs(0).asInstanceOf[T])))

  /*------------------------- EBNF synthesis --------------------------*/

  // x.*, x.?, ... synthesize ordinary rules (memoized so the same
  // modifier on the same symbol is the same nonterminal).  The list
  // forms are right-recursive, the LL(1)-friendly direction; the
  // engine runs the ones marked with a LoopKind iteratively, so long
  // lists cannot overflow the stack.  Their names are derived lazily
  // so they read correctly after rule-name reflection.
  private val synthCache = collection.mutable.Map[(Sym[?], Sym[?], String), Rule[?]]()

  private def synthRule[A](nameFn: () => String, loop: LoopKind,
                           ps: () => Seq[Prod[A]]): Rule[A] =
    new Rule(nameFn, true, ps, greedy = true, loop)

  // The synthesized repetition rules are greedy (see `*`); opt and
  // user-written rules stay strict.
  private def starRule[A](s: Sym[A]): Rule[List[A]] =
    synthCache.getOrElseUpdate((s, s, "*"), {
      lazy val r: Rule[List[A]] = synthRule(() => s.toString + "*", LoopKind.Star,
        () => Seq(
          mkProd(List(s, r),
            vs => vs(0).asInstanceOf[A] :: vs(1).asInstanceOf[List[A]]),
          mkProd(Nil, _ => Nil)))
      r
    }).asInstanceOf[Rule[List[A]]]

  private def rep1Rule[A](s: Sym[A]): Rule[List[A]] =
    synthCache.getOrElseUpdate((s, s, "+"), {
      new Rule(() => s.toString + "+", true,
        () => Seq(mkProd(List(s, starRule(s)),
          vs => vs(0).asInstanceOf[A] :: vs(1).asInstanceOf[List[A]])),
        greedy = false)
    }).asInstanceOf[Rule[List[A]]]

  private def optRule[A](s: Sym[A]): Rule[Option[A]] =
    synthCache.getOrElseUpdate((s, s, "?"), {
      new Rule(() => s.toString + "?", true,
        () => Seq(
          mkProd(List(s), vs => Some(vs(0).asInstanceOf[A])),
          mkProd(Nil, _ => None)),
        greedy = false)
    }).asInstanceOf[Rule[Option[A]]]

  private def repSepRule[A](s: Sym[A], sep: Sym[?]): Rule[List[A]] =
    synthCache.getOrElseUpdate((s, sep, "%*"), {
      lazy val tail: Rule[List[A]] =
        synthRule(() => s"(${sep.toString} ${s.toString})*", LoopKind.SepTail,
          () => Seq(
            mkProd(List(sep, s, tail),
              vs => vs(1).asInstanceOf[A] :: vs(2).asInstanceOf[List[A]]),
            mkProd(Nil, _ => Nil)))
      new Rule(() => s"${s.toString}%${sep.toString}*", true,
        () => Seq(
          mkProd(List(s, tail),
            vs => vs(0).asInstanceOf[A] :: vs(1).asInstanceOf[List[A]]),
          mkProd(Nil, _ => Nil)),
        greedy = false)
    }).asInstanceOf[Rule[List[A]]]

  /*----------------------------- sugar -------------------------------*/

  /**
   * A level of left-associative binary operators: `operand (op
   * operand)*`, folded to the left, with `combine(left, opToken,
   * right)` building each node.  This is the LL(1) replacement for
   * yacc's %left -- the left-recursive rule you would write for +,
   * -, *, / is illegal here, but the tree still leans left:
   * 9-2-3 parses as combine(combine(9,-,2),-,3).
   */
  def leftAssoc[A](operand: => Sym[A])(ops: K*)(combine: (A, T, A) => A): Rule[A] =
    mkLeftAssoc(null, operand, ops, combine)
  def leftAssoc[A](name: String, operand: => Sym[A])(ops: K*)(
      combine: (A, T, A) => A): Rule[A] =
    mkLeftAssoc(name, operand, ops, combine)

  private def mkLeftAssoc[A](name: String | Null, operand: => Sym[A], ops: Seq[K],
                             combine: (A, T, A) => A): Rule[A] =
    lazy val result: Rule[A] = {
      val opPair: Rule[(T, A)] = synthRule(() => s"(${result.name}-op)", LoopKind.None,
        () => ops.map(k => mkProd(List(Term(k), operand),
          vs => (vs(0).asInstanceOf[T], vs(1).asInstanceOf[A]))))
      mkRule(name, false, LoopKind.None, () => Seq(
        mkProd(List(operand, starRule(opPair)),
          vs => vs(1).asInstanceOf[List[(T, A)]]
                  .foldLeft(vs(0).asInstanceOf[A]) {
                    case (acc, (t, next)) => combine(acc, t, next) })))
    }
    result

  /** Like leftAssoc, but folding rightward: a op (b op c). */
  def rightAssoc[A](operand: => Sym[A])(ops: K*)(combine: (A, T, A) => A): Rule[A] =
    lazy val result: Rule[A] = {
      val opPair: Rule[(T, A)] = synthRule(() => s"(${result.name}-op)", LoopKind.None,
        () => ops.map(k => mkProd(List(Term(k), operand),
          vs => (vs(0).asInstanceOf[T], vs(1).asInstanceOf[A]))))
      def foldR(first: A, rest: List[(T, A)]): A = rest match
        case Nil            => first
        case (t, a) :: tail => combine(first, t, foldR(a, tail))
      mkRule(null, false, LoopKind.None, () => Seq(
        mkProd(List(operand, starRule(opPair)),
          vs => foldR(vs(0).asInstanceOf[A], vs(1).asInstanceOf[List[(T, A)]]))))
    }
    result

  /** One level of an `operators` precedence table. */
  final case class Level private[Grammar] (kinds: Seq[K], rightAssociative: Boolean)
  /** A left-associative level (yacc's %left), lowest precedence first. */
  def left(ks: K*): Level = Level(ks, rightAssociative = false)
  /** A right-associative level (yacc's %right). */
  def right(ks: K*): Level = Level(ks, rightAssociative = true)

  /**
   * A whole precedence ladder in one declaration -- the closest LL(1)
   * gets to yacc's %left/%right block.  Levels are given from lowest
   * to highest precedence; each becomes one rule over the next:
   *
   *   lazy val expr: Rule[E] = operators(unary)(
   *     left(OR), left(AND), left(PLUS, MINUS), left(MULT, DIV))(binOp)
   */
  def operators[A](operand: => Sym[A])(levels: Level*)(
      combine: (A, T, A) => A): Rule[A] =
    require(levels.nonEmpty, "operators needs at least one level")
    lazy val outermost: Rule[A] = {
      def build(ls: List[Level], depth: Int): Rule[A] =
        val inner: () => Sym[A] =
          ls.tail match
            case Nil  => () => operand
            case rest => { lazy val r = build(rest, depth + 1); () => r }
        val mk = if ls.head.rightAssociative then
          rightAssoc[A](inner())(ls.head.kinds*)(combine)
        else
          mkLeftAssoc[A](null, inner(), ls.head.kinds, combine)
        if depth > 0 then mk.nameFn = () => s"${outermost.name}@$depth"
        mk
      build(levels.toList, 0)
    }
    outermost

  /*--------------------------- entry points --------------------------*/

  /**
   * Parse a complete token sequence starting from `start`.  The whole
   * input must be consumed.  Throws GrammarError if the grammar is
   * not LL(1), ParseError if the input does not match.
   *
   * `trace = true` narrates the parse step by step to stdout: the
   * numbered production listing (same numbers as `dump()`), then each
   * prediction, terminal match, list-loop decision, and rule result
   * as it happens -- for debugging a grammar, and for watching the
   * LL(1) algorithm run.  A `String => Unit` sink can stand in for
   * `true` to receive the lines instead (`trace = println` is the
   * spelled-out equivalent).
   */
  def parse[A](start: Rule[A], toks: IndexedSeq[T],
               trace: Trace = false): A =
    run(start, toks, Trace.sink(trace))
