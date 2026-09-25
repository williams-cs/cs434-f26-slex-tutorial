package scup

/**
 * What a parse's `trace` argument accepts: `true` narrates the parse
 * to stdout; a `String => Unit` sink receives the lines instead
 * (`trace = println` is the spelled-out equivalent of `trace = true`;
 * tools capture traces by passing a buffer's append).
 */
type Trace = Boolean | (String => Unit)

private[scup] object Trace:
  /** The line sink a trace argument denotes; null when tracing is off. */
  def sink(t: Trace): String => Unit = t match
    case b: Boolean => if b then s => Console.println(s) else null
    case f          => f.asInstanceOf[String => Unit]

/**
 * The predictive engine: parse by consulting the LL(1) table on one
 * token of lookahead.  No backtracking ever happens -- at each rule
 * exactly one production is selected (or a ParseError is thrown that
 * lists every terminal that could have appeared).
 *
 * The synthesized list rules (`x.rep`, `x.repSep(sep)`) are
 * right-recursive for the analysis but run here as loops, so long
 * lists cost constant stack.
 *
 * With a trace sink installed (`parse(start, toks, trace = println)`)
 * the engine narrates every step it takes: the numbered production
 * listing up front (the same numbers as `dump()`), then one line per
 * prediction, terminal match, list-loop decision, and rule result --
 * the algorithm from lecture, watchable.
 */
private[scup] trait GrammarEngine[T <: Tokenish, K]:
  this: Grammar[T, K] =>

  /**
   * Parse a complete token sequence starting from `start`; the whole
   * input must be consumed.  Throws GrammarError if the grammar is
   * not LL(1), ParseError if the input does not match it.  A non-null
   * `trace` receives the step-by-step narration, one line per call.
   */
  private[scup] def run[A](start: Rule[A], toks: IndexedSeq[T],
                           trace: String => Unit = null): A =
    val a  = analysis(start)
    var in = Input(toks)

    def la: Option[K] = in.current.map(kindOf)

    /*----------------------------- tracing ----------------------------*/

    var depth = 0
    def emit(line: String): Unit = trace("  " * depth + line)

    def tokenText(t: T): String = t match
      case tok: Token[?] => tok.text
      case _             => null

    // The lookahead, printable: kind, text when it adds anything
    // (fixed-text kinds already display as their spelling), position.
    def laShow: String = in.current match
      case None => "end of input"
      case Some(t) =>
        val k = showKind(kindOf(t))
        val text = tokenText(t)
        val spell = if text != null && k != s"'$text'" then s" '$text'" else ""
        s"$k$spell @${in.pos.line}:${in.pos.col}"

    // A value on a rule-exit line, elided so big ASTs stay readable.
    def short(v: Any): String =
      val s = try String.valueOf(v) catch case _: Exception => "<toString failed>"
      if s.length <= 60 then s else s.take(57) + "..."

    if trace != null then
      trace(a.productionListing)
      trace(s"parse: ${short(toks.map(t => Option(tokenText(t)).getOrElse(t.toString)).mkString(" "))}")
      trace("")

    /*----------------------------- parsing ----------------------------*/

    def parseTerm(k: K, inRule: Rule[?]): T =
      in.current match
        case Some(t) if kindOf(t) == k =>
          if trace != null then emit(s"match $laShow")
          in = in.advance; t
        case _ => throw ParseError(List(showKind(k)), in.pos, inRule.name)

    def parseSym(s: Sym[?], inRule: Rule[?]): Any = s match
      case Term(k)      => parseTerm(k, inRule)
      case r: Rule[?]   => parseRule(r)

    def choose(r: Rule[?]): Int =
      a.table(r).getOrElse(la, throw ParseError(a.expected(r), in.pos, r.name))

    def nested[B](body: => B): B =
      depth += 1
      try body finally depth -= 1

    def parseRule[B](r: Rule[B]): B =
      val result: Any = r.loop match
        case LoopKind.Star =>
          // prods(0) = [s, this]; keep taking s while the table says so
          val s   = r.prods(0).syms.head
          val buf = List.newBuilder[Any]
          while a.table(r).get(la).contains(0) do
            if trace != null then emit(s"${r.name}: $laShow continues loop")
            nested { buf += parseSym(s, r) }
          if trace != null then emit(s"${r.name}: $laShow stops loop")
          buf.result()
        case LoopKind.SepTail =>
          // prods(0) = [sep, s, this]
          val sep = r.prods(0).syms(0)
          val s   = r.prods(0).syms(1)
          val buf = List.newBuilder[Any]
          while a.table(r).get(la).contains(0) do
            if trace != null then emit(s"${r.name}: $laShow continues loop")
            nested {
              parseSym(sep, r)
              buf += parseSym(s, r)
            }
          if trace != null then emit(s"${r.name}: $laShow stops loop")
          buf.result()
        case LoopKind.None =>
          val idx = choose(r)
          if trace != null then
            val name = r.name.padTo(math.max(14 - 2 * depth, r.name.length), ' ')
            emit(s"$name ${laShow.padTo(16, ' ')} predict (${a.prodNum((r, idx))})")
          val p    = r.prods(idx)
          val vals = new Array[Any](p.syms.length)
          nested {
            var i = 0
            for s <- p.syms do
              vals(i) = parseSym(s, r)
              i += 1
          }
          val v = p.act(collection.immutable.ArraySeq.unsafeWrapArray(vals))
          if trace != null then emit(s"${r.name} = ${short(v)}")
          v
      result.asInstanceOf[B]

    try
      val result = parseRule(start)
      if !in.atEnd then
        throw ParseError(List("end of input"), in.pos, start.name)
      result
    catch case e: ParseError =>
      if trace != null then emit(s"parse error: ${e.getMessage}")
      throw e
