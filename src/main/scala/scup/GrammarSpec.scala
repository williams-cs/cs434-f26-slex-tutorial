package scup

import SpecAssemble.*
import java.lang.reflect.{InvocationTargetException, Method}
import java.nio.file.Files

/**
 * A grammar loaded from a `.scup` spec file -- the yacc-style clean
 * spec surface over the scup engine.  A second front end: it builds
 * exactly the Rule/Prod structures the embedded DSL builds, so LL(1)
 * analysis, conflict diagnostics, `dump()`, and `ParseError` work
 * unchanged.
 *
 *   val lex = LexSpec.load[TokenKind]("ic.slex")
 *   val g   = GrammarSpec.load("ic.scup", lex.binding)
 *   val ast = g.parseAs[ProgramNode](lex.tokenize(source))
 */
final class DynamicGrammar[K](
    val model: ScupSpecModel,
    binding: TokenBinding[K],
    actionMethods: IndexedSeq[Method],
    actionInstance: AnyRef)
    extends Grammar[Token[K], K](_.kind, binding.kindNames):

  private def err(line: Int, msg: String): Nothing =
    throw new SpecError(model.path, line, msg)

  private def invoke(n: Int, args: Seq[Any]): Any =
    try actionMethods(n).invoke(actionInstance, args.map(_.asInstanceOf[AnyRef])*)
    catch case e: InvocationTargetException =>
      throw SpecAssemble.respec(e.getCause, model.objectName, model.path)

  // actionN numbering mirrors SpecAssemble.assembleScup: blocks in
  // rule order, alternative order.
  private val actionIndex: Map[(String, Int), Int] =
    val b = Map.newBuilder[(String, Int), Int]
    var n = -1
    for r <- model.rules do
      for (a, ai) <- r.alts.zipWithIndex do
        if a.block.isDefined then
          n += 1
          b += (((r.name, ai), n))
    b.result()

  private val ruleDefs: Map[String, RuleDef] = model.rules.map(r => r.name -> r).toMap
  private val suffixNames: Set[String] = model.chainSuffixes

  val startName: String = model.start.getOrElse(model.rules.head.name)

  validateRefs()

  private val ruleCache = collection.mutable.Map[String, Rule[Any]]()
  private val modCache = collection.mutable.Map[(AnyRef, String), Sym[Any]]()

  /** The named rule, as a start symbol or for inspection. */
  def ruleFor(name: String): Rule[Any] =
    ruleCache.getOrElseUpdate(name, {
      val rd = ruleDefs.getOrElse(name,
        throw SpecAssemble.diagnostic(
          new NoSuchElementException(s"no rule '$name' in ${model.path}")))
      if suffixNames.contains(name) then buildSuffixRule(rd) else buildRule(rd)
    })

  lazy val start: Rule[Any] = ruleFor(startName)

  /** Parse a complete token sequence from the start rule.  Pass
   *  `trace = true` to watch every step the parser takes (or a
   *  `String => Unit` sink to receive the lines). */
  def parseAll(toks: IndexedSeq[Token[K]], trace: Trace = false): Any =
    parse(start, toks, trace)
  def parseAs[A](toks: IndexedSeq[Token[K]], trace: Trace = false): A =
    parseAll(toks, trace).asInstanceOf[A]

  /** Check the grammar is LL(1) (throws GrammarError with the DSL's
   *  exact diagnostics). */
  def check(): Unit = start.check()

  /** FIRST/FOLLOW sets and the LL(1) table, as a printable string. */
  def dump(): String = start.dump()

  /*--------------------------- symbol resolution --------------------------*/

  private def kindOfTerm(s: SymRef): K = s match
    case SName(nm, _, line) =>
      binding.kinds.getOrElse(nm,
        err(line, s"'$nm' is neither a rule nor a member of the token-kind enum"))
    case SLit(tx, _, line) =>
      binding.literals.getOrElse(tx,
        err(line, s"\"$tx\" is not in the lexer spec's %token table"))
    case SOneOf(_, _, line) => err(line, "nested (...) groups are not supported")

  private def baseSym(s: SymRef): Sym[Any] = s match
    case SName(nm, _, _) if ruleDefs.contains(nm) => ruleFor(nm)
    case SOneOf(alts, _, line) =>
      val desc = "(" + alts.map(a => showKind(kindOfTerm(a))).mkString(" | ") + ")"
      mkRule(desc, false, LoopKind.None,
        () => alts.map(a => mkProd(List(Term(kindOfTerm(a))), vs => vs(0))))
    case other => Term(kindOfTerm(other))

  private def resolve(s: SymRef): Sym[Any] =
    var b = baseSym(s)
    for m <- s.mods do
      b = m match
        case MOpt          => optOf(b)
        case MStar         => starOf(b)
        case MPlus         => plusOf(b)
        case MStarSep(sep) => sepOf(b, baseSym(sep))
    b

  // The synthesized modifier rules: like the DSL's -- x? yields
  // Option (Some/None, exactly as the DSL's optRule does) and the
  // list forms yield List.
  private def optOf(s: Sym[Any]): Sym[Any] =
    modCache.getOrElseUpdate((s, "?"), {
      new Rule[Any](() => s.toString + "?", true,
        () => Seq(mkProd(List(s), vs => Some(vs(0))), mkProd(Nil, _ => None)),
        greedy = false, LoopKind.None)
    })

  private def starOf(s: Sym[Any]): Sym[Any] =
    modCache.getOrElseUpdate((s, "*"), {
      lazy val r: Rule[Any] = new Rule(() => s.toString + "*", true,
        () => Seq(
          mkProd(List(s, r), vs => vs(0) :: vs(1).asInstanceOf[List[Any]]),
          mkProd(Nil, _ => Nil)),
        greedy = true, LoopKind.Star)
      r
    })

  private def plusOf(s: Sym[Any]): Sym[Any] =
    modCache.getOrElseUpdate((s, "+"), {
      new Rule[Any](() => s.toString + "+", true,
        () => Seq(mkProd(List(s, starOf(s)),
          vs => vs(0) :: vs(1).asInstanceOf[List[Any]])),
        greedy = false, LoopKind.None)
    })

  private def sepOf(s: Sym[Any], sep: Sym[Any]): Sym[Any] =
    modCache.getOrElseUpdate((s, "%*" + sep.toString), {
      lazy val tail: Rule[List[Any]] = new Rule(() => s"($sep $s)*", true,
        () => Seq(
          mkProd(List(sep, s, tail),
            vs => vs(1) :: vs(2).asInstanceOf[List[Any]]),
          mkProd(Nil, _ => Nil)),
        greedy = true, LoopKind.SepTail)
      new Rule[Any](() => s"%list($s, $sep)", true,
        () => Seq(
          mkProd(List(s, tail), vs => vs(0) :: vs(1).asInstanceOf[List[Any]]),
          mkProd(Nil, _ => Nil)),
        greedy = false, LoopKind.None)
    })

  /*---------------------------- rule building -----------------------------*/

  private def buildRule(rd: RuleDef): Rule[Any] = rd.alts match
    case List(ABinary(operand, levels, _, line)) =>
      val n = actionIndex.getOrElse((rd.name, 0),
        err(line, "%binary needs an action block (the combine)"))
      val opSym = baseSym(SName(operand, Nil, line))
      val lvls = levels.map { l =>
        val ks = l.elems.map(kindOfTerm)
        if l.rightAssoc then right(ks*) else left(ks*)
      }
      val r = operators[Any](opSym)(lvls*)((a, t, b) => invoke(n, Seq(a, t, b)))
      r.nameFn = () => rd.name
      r.named = true
      r
    case _ =>
      for a <- rd.alts do
        if a.isInstanceOf[ABinary] then
          err(a.line, "%binary must be the rule's only alternative")
      val bare = model.isBare(rd)
      mkRule(rd.name, rd.greedy, LoopKind.None,
        () => rd.alts.zipWithIndex.map((a, ai) => prodFor(rd, a, ai, bare)))

  private def prodFor(rd: RuleDef, a: AltDef, ai: Int, bare: Boolean): Prod[Any] =
    a match
      case AEmpty(Some(_), _) =>
        val n = actionIndex((rd.name, ai))
        mkProd(Nil, _ => invoke(n, Nil))
      case AEmpty(None, _) =>
        if bare then mkProd(Nil, _ => STree(rd.name, Nil, Pos(0, 0)))
        else mkProd(Nil, _ => None)   // the un-blocked %empty: absent
      case ASyms(syms, blockOpt, line) =>
        val resolved = syms.map(resolve)
        blockOpt match
          case Some(_) =>
            val n = actionIndex((rd.name, ai))
            mkProd(resolved, vs => invoke(n, vs))
          case None =>
            if syms.length == 1 then mkProd(resolved, vs => vs(0))
            else if bare then
              // In a bare tree an absent optional contributes no child.
              mkProd(resolved, vs => {
                val kids = vs.toList.flatMap {
                  case None    => Nil
                  case Some(v) => List(v)
                  case v       => List(v)
                }
                STree(rd.name, kids, STree.firstPos(kids))
              })
            else err(line,
              "a production with more than one symbol needs an action block")
      case AChain(base, suffix, _) =>
        val b = resolve(base)
        val sufStar = starOf(ruleFor(suffix))
        mkProd(List(b, sufStar), vs =>
          vs(1).asInstanceOf[List[Any]].foldLeft(vs(0))((acc, f) =>
            f.asInstanceOf[Any => Any](acc)))
      case _: ABinary =>
        err(a.line, "%binary must be the rule's only alternative") // unreachable

  /** A %chain suffix rule: its productions build `acc => value`
   *  closures; `$$` reaches the action as an extra first argument. */
  private def buildSuffixRule(rd: RuleDef): Rule[Any] =
    mkRule(rd.name, rd.greedy, LoopKind.None,
      () => rd.alts.zipWithIndex.map { (a, ai) =>
        val n = actionIndex.getOrElse((rd.name, ai),
          err(a.line, s"every production of %chain suffix rule '${rd.name}' " +
            "needs an action block"))
        a match
          case AEmpty(_, _) =>
            mkProd(Nil, _ => ((acc: Any) => invoke(n, Seq(acc))): Any)
          case ASyms(syms, _, _) =>
            val resolved = syms.map(resolve)
            mkProd(resolved, vs => ((acc: Any) => invoke(n, acc +: vs)): Any)
          case other =>
            err(other.line, "%binary/%chain cannot appear in a %chain suffix rule")
      })

  /*------------------------- load-time validation -------------------------*/

  private def validateRefs(): Unit =
    def checkTerm(s: SymRef): Unit = s match
      case SName(nm, _, line) =>
        if !binding.kinds.contains(nm) then
          err(line, s"'$nm' is not a member of the token-kind enum" +
            GrammarSpec.suggest(nm, binding.kinds.keys))
      case SLit(tx, _, line) =>
        if !binding.literals.contains(tx) then
          err(line, s"\"$tx\" is not in the lexer spec's %token table" +
            GrammarSpec.suggestLit(tx, binding.literals.keys))
      case SOneOf(_, _, line) => err(line, "nested (...) groups are not supported")
    def checkSym(s: SymRef): Unit =
      s match
        case SName(nm, _, line) =>
          if suffixNames.contains(nm) then
            err(line, s"'$nm' is a %chain suffix rule and cannot appear " +
              "as an ordinary symbol")
          if !ruleDefs.contains(nm) && !binding.kinds.contains(nm) then
            err(line, s"'$nm' is neither a rule nor a member of the token-kind enum" +
              GrammarSpec.suggest(nm, ruleDefs.keys ++ binding.kinds.keys))
        case SLit(tx, _, line) =>
          if !binding.literals.contains(tx) then
            err(line, s"\"$tx\" is not in the lexer spec's %token table" +
              GrammarSpec.suggestLit(tx, binding.literals.keys))
        case SOneOf(alts, _, _) => alts.foreach(checkTerm)
      s.mods.foreach {
        case MStarSep(sep) => checkSym(sep)
        case _             => ()
      }
    for r <- model.rules; a <- r.alts do a match
      case AEmpty(None, line) =>
        // The un-blocked %empty produces None, so the rule's declared
        // type must be an Option (or stay undeclared, in bare style).
        for td <- model.types.get(r.name)
            if !td.tpe.trim.startsWith("Option[") do
          err(line, s"'${r.name}' has an un-blocked %empty, which produces " +
            s"None -- declare '%type ${r.name}: Option[...]' or give the " +
            "%empty alternative a block")
      case ASyms(syms, _, _) => syms.foreach(checkSym)
      case ABinary(op, levels, _, line) =>
        if !ruleDefs.contains(op) && !binding.kinds.contains(op) then
          err(line, s"'$op' is neither a rule nor a member of the token-kind enum" +
            GrammarSpec.suggest(op, ruleDefs.keys ++ binding.kinds.keys))
        for l <- levels; e <- l.elems do checkTerm(e)
      case AChain(base, suf, line) =>
        checkSym(base)
        val sufDef = ruleDefs.getOrElse(suf,
          err(line, s"'$suf' (the %chain suffix) is not a rule"))
        for sa <- sufDef.alts do
          if sa.block.isEmpty then
            err(sa.line, s"every production of %chain suffix rule '$suf' " +
              "needs an action block")
      case _ => ()
    for (nm, td) <- model.types do
      if !ruleDefs.contains(nm) then
        err(td.line, s"%type names unknown rule '$nm'" +
          GrammarSpec.suggest(nm, ruleDefs.keys))
    if !ruleDefs.contains(startName) then
      err(1, s"start rule '$startName' is not defined")
    for
      r <- model.rules
      if !model.isBare(r) && !suffixNames.contains(r.name)
      a <- r.alts
    do
      a match
        case ASyms(syms, None, line) if syms.length > 1 =>
          val why =
            if model.types.contains(r.name) then
              s"'${r.name}' has a %type"
            else if r.alts.exists(x => x.isInstanceOf[ABinary] || x.isInstanceOf[AChain]) then
              s"'${r.name}' uses %binary/%chain"
            else
              s"other productions of '${r.name}' have action blocks"
          err(line, s"this production needs an action block: $why, so the " +
            "rule produces a value, and every multi-symbol production must " +
            "say which value it makes (only a rule left entirely action-free " +
            "-- no blocks, no %type -- can go without)")
        case _ => ()

object GrammarSpec:

  /*----------------------- novice-friendly hints ------------------------*/

  private def editDistance(a: String, b: String): Int =
    val d = Array.tabulate(a.length + 1, b.length + 1)((i, j) =>
      if i == 0 then j else if j == 0 then i else 0)
    for i <- 1 to a.length; j <- 1 to b.length do
      d(i)(j) = math.min(
        math.min(d(i - 1)(j) + 1, d(i)(j - 1) + 1),
        d(i - 1)(j - 1) + (if a(i - 1) == b(j - 1) then 0 else 1))
    d(a.length)(b.length)

  private def nearest(name: String, candidates: Iterable[String]): Option[String] =
    val limit = if name.length <= 4 then 1 else 2
    candidates.iterator
      .filter(_ != name)
      .map(c => (c, editDistance(name.toLowerCase, c.toLowerCase)))
      .filter(_._2 <= limit)
      .minByOption(_._2)
      .map(_._1)

  /** " -- did you mean 'x'?" when a close candidate exists, else "". */
  def suggest(name: String, candidates: Iterable[String]): String =
    nearest(name, candidates).map(c => s" -- did you mean '$c'?").getOrElse("")

  def suggestLit(text: String, candidates: Iterable[String]): String =
    nearest(text, candidates).map(c => " -- did you mean \"" + c + "\"?").getOrElse("")

  /** Hints appended to action-compile failures: `Not found: $N` means
   *  the block names a symbol its production does not have, and
   *  `Not found: $$` means the block is not in a %chain suffix rule.
   *  The compiler's positions are spec lines, so the offending
   *  production (and its symbol count) can be named exactly. */
  private def dollarHints(model: SpecAssemble.ScupSpecModel, msg: String): String =
    import SpecAssemble.*
    def altAt(line: Int): Option[(RuleDef, AltDef)] =
      (for
        r <- model.rules
        a <- r.alts
        b <- a.block
        if b.openLine <= line && line <= b.closeLine
      yield (r, a)).headOption
    def symCount(a: AltDef): Int = a match
      case ASyms(syms, _, _) => syms.length
      case _: ABinary        => 3
      case _                 => 0
    val hints = collection.mutable.LinkedHashSet[String]()
    var lastLine = 0
    val loc = raw":(\d+):\d+".r
    // dotc renders an undefined `$3` as "Not found: $3" but an
    // undefined `$$` as just "Not found: $"
    val notFound = raw"Not found: \$$(\d+|(?![\w$$]))".r
    for l <- msg.linesIterator do
      loc.findFirstMatchIn(l).foreach(m => lastLine = m.group(1).toInt)
      for m <- notFound.findFirstMatchIn(l) do
        val what = if m.group(1) == null || m.group(1).isEmpty then "$" else m.group(1)
        altAt(lastLine) match
          case Some((r, a)) if what == "$" =>
            hints += s"hint: $$$$ (the value built so far) is available only " +
              s"inside a %chain suffix rule's blocks, and '${r.name}' is not one"
          case Some((r, a)) =>
            hints += s"hint: the production at line ${a.line} has ${symCount(a)} " +
              s"symbol(s), so its block can use $$1..$$${symCount(a)} only"
          case None if what == "$" =>
            hints += "hint: $$ is available only inside a %chain suffix rule's blocks"
          case None =>
            hints += "hint: $N names the Nth symbol of the production, " +
              "counting every symbol"
    if hints.isEmpty then "" else hints.mkString("\n", "\n", "")

  /** The idents a standalone load treats as terminals because no rule
   *  defines them.  The check CLI flags suspicious ones: a misspelled
   *  nonterminal silently becomes a terminal otherwise. */
  def impliedTerminals(model: SpecAssemble.ScupSpecModel): List[String] =
    import SpecAssemble.*
    val names = collection.mutable.LinkedHashSet[String]()
    def scanSym(s: SymRef): Unit =
      s match
        case SName(nm, _, _) => if !model.ruleNames.contains(nm) then names += nm
        case SOneOf(alts, _, _) => alts.foreach(scanSym)
        case _ => ()
      s.mods.foreach { case MStarSep(sep) => scanSym(sep); case _ => () }
    for r <- model.rules; a <- r.alts do a match
      case ASyms(syms, _, _) => syms.foreach(scanSym)
      case ABinary(op, levels, _, _) =>
        if !model.ruleNames.contains(op) then names += op
        for l <- levels; e <- l.elems do scanSym(e)
      case AChain(base, _, _) => scanSym(base)
      case _ => ()
    names.toList

  /** Load a grammar spec against a token binding (usually
   *  `LexSpec.load[K](...).binding`). */
  def load[K](path: String, binding: TokenBinding[K]): DynamicGrammar[K] =
    val model = SpecAssemble.parseScup(path, SpecFile.readText(path))
    val nActions = model.rules.flatMap(_.alts).count(_.block.isDefined)
    val (methods, inst) =
      if nActions == 0 then (Vector.empty[Method], null)
      else
        val loaded = DynCompile.findCompiled(model.objectName, model.hash)
          .getOrElse {
            val src = SpecAssemble.assembleScup(model, binding.kindsFqcn)
            try DynCompile.compileAndLoad(model.objectName, src)
            catch case e: RuntimeException =>
              val msg = e.getMessage.replace(model.objectName + ".scala", path)
              throw SpecAssemble.diagnostic(
                new RuntimeException(msg + dollarHints(model, msg), e))
          }
        val ms = loaded.cls.getMethods
          .filter(_.getName.matches("action\\d+"))
          .sortBy(_.getName.drop("action".length).toInt)
          .toIndexedSeq
        if ms.length != nActions then
          throw SpecAssemble.diagnostic(new RuntimeException(
            s"$path: stale actions object (${ms.length} actions compiled, " +
            s"$nActions in the spec) -- rebuild"))
        (ms, loaded.instance)
    new DynamicGrammar[K](model, binding, methods, inst)

  /**
   * Host-less load for bare (action-free) grammars: every terminal
   * becomes its own synthetic kind, so the grammar LL(1)-checks and
   * `dump()`s with no lexer spec and no enum -- the HW grammar
   * self-check mode.
   */
  def loadStandalone(path: String): DynamicGrammar[String] =
    val model = SpecAssemble.parseScup(path, SpecFile.readText(path))
    val names = impliedTerminals(model)
    val lits = collection.mutable.LinkedHashSet[String]()
    def scanLits(s: SymRef): Unit =
      s match
        case SLit(tx, _, _)     => lits += tx
        case SOneOf(alts, _, _) => alts.foreach(scanLits)
        case _                  => ()
      s.mods.foreach { case MStarSep(sep) => scanLits(sep); case _ => () }
    for r <- model.rules; a <- r.alts do a match
      case ASyms(syms, _, _) => syms.foreach(scanLits)
      case ABinary(_, levels, _, _) =>
        for l <- levels; e <- l.elems do scanLits(e)
      case AChain(base, _, _) => scanLits(base)
      case _ => ()
    val binding = TokenBinding[String](
      kinds = names.map(n => n -> n).toMap,
      literals = lits.map(t => t -> ("'" + t + "'")).toMap,
      kindNames = (names.map(n => n -> n) ++ lits.map(t => ("'" + t + "'") -> ("'" + t + "'"))).toMap,
      kindsFqcn = "java.lang.String")
    load(path, binding)

