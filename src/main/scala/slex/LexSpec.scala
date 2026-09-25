package slex

import scup.{DynCompile, KindBind, SpecAssemble, Token}
import scup.SpecAssemble.{FixedRule, LexSpecModel, PatRule, SpecError}
import scala.reflect.ClassTag

/**
 * A lexer loaded from a `.slex` spec file -- the lex/yacc-style clean
 * spec surface over the slex engine.  The spec's rules become exactly
 * the `Rule` list the embedded DSL would build; maximal munch and
 * first-rule-wins are unchanged.
 *
 *   val spec = LexSpec.load[TokenKind]("ic.slex")
 *   val toks = spec.tokenize(source)
 *
 * Also carries the `%token` literal table, which is what lets a
 * companion `.scup` grammar write `"while"` for a terminal and lets
 * parse errors display kinds as their source text.
 */
final class LexSpec[K](
    val lexer: Lexer[Token[K]],
    val kinds: Map[String, K],     // enum member name -> kind
    val literals: Map[String, K],  // %token text -> kind
    val kindNames: Map[K, String], // kind -> "'text'" display name
    val kindsFqcn: String,
    val model: LexSpecModel):

  /** Both take scup-style `trace` arguments: `true` (or a
   *  `String => Unit` sink) narrates every munch -- and an `engine`,
   *  the DFA by default or the NFA simulation -- see Lexer. */
  def tokenize(src: String, trace: scup.Trace = false,
               engine: Engine = Engine.Dfa): IndexedSeq[Token[K]] =
    lexer.tokenize(src, trace, engine)
  def scan(src: String, trace: scup.Trace = false,
           engine: Engine = Engine.Dfa): lexer.Scan =
    lexer.scan(src, trace, engine)

  /** The spec's rules, its combined NFA, and the DFA built from it,
   *  printable -- what `slex.check -dump` prints. */
  def dump(): String =
    val sb = StringBuilder()
    sb ++= "Rules (listing order breaks ties):\n"
    val rows: List[(String, String, Int)] = model.rules.map {
      case FixedRule(kind, text, line) => (s"\"$text\"", s"-> $kind", line)
      case PatRule(pat, line, Some(_)) => (pat, "{ action }", line)
      case PatRule(pat, line, None)    => (pat, "(skip)", line)
    }
    val w = (0 :: rows.map(_._1.length)).max
    for ((pat, what, line), i) <- rows.zipWithIndex do
      sb ++= f"  #${i + 1}%-3d ${pat.padTo(w, ' ')}  $what%-12s (line $line)%n"
    sb ++= "\n"
    sb ++= lexer.nfa.dump(lexer.label)
    sb ++= "\n"
    sb ++= lexer.dfa.dump(lexer.label)
    sb.result()

  /** What a companion `.scup` grammar needs from this token language. */
  def binding: scup.TokenBinding[K] =
    scup.TokenBinding(kinds, literals, kindNames, kindsFqcn)

object LexSpec:

  /** Load a spec, binding kind names to the members of enum K. */
  def load[K](path: String)(using ct: ClassTag[K]): LexSpec[K] =
    loadBound[K](path, KindBind.kindMap[K], ct.runtimeClass.getName)

  /** Host-less load: kinds come from the spec's `%kinds` declaration,
   *  bound by reflection (the check CLI, table dumpers). */
  def loadReflect(path: String): LexSpec[Any] =
    val (text, model) = read(path)
    val fqcn = model.kinds.getOrElse(
      throw new SpecError(path, 1, "host-less load requires a %kinds declaration"))
    finish[Any](path, model, KindBind.kindMapReflect(fqcn), fqcn)

  def loadBound[K](path: String, kindMap: Map[String, K],
                   kindsFqcn: String): LexSpec[K] =
    val (text, model) = read(path)
    for k <- model.kinds do
      if k != kindsFqcn then
        throw new SpecError(path, declLine(model, "%kinds"),
          s"%kinds declares $k but the loader binds $kindsFqcn")
    finish[K](path, model, kindMap, kindsFqcn)

  private def read(path: String): (String, LexSpecModel) =
    val text = scup.SpecFile.readText(path)
    (text, SpecAssemble.parseLex(path, text))

  private def declLine(model: LexSpecModel, decl: String): Int =
    val i = model.lines.indexWhere(_.trim.startsWith(decl))
    if i < 0 then 1 else i + 1

  private def finish[K](path: String, model: LexSpecModel,
                        kindMap: Map[String, K], kindsFqcn: String): LexSpec[K] =
    validate(path, model, kindMap)

    // parse every pattern up front, so errors carry rule lines
    val compiledRules: List[(SpecAssemble.SRule, Re)] =
      model.rules.map {
        case r @ FixedRule(_, text, _) => (r, Re.str(text))
        case r @ PatRule(pattern, line, _) =>
          try (r, ReSyntax.parse(pattern))
          catch case e: IllegalArgumentException =>
            throw new SpecError(path, line, e.getMessage)
      }

    // bind the actions object: Path A (already compiled) or Path B
    val nActions = model.actionBlocks.length
    val actions: IndexedSeq[(String, scup.Pos) => Token[K]] =
      if nActions == 0 then Vector.empty
      else
        val loaded = DynCompile.findCompiled(model.objectName, model.hash)
          .getOrElse {
            val src = SpecAssemble.assembleLex(model, model.kinds.getOrElse(kindsFqcn))
            try DynCompile.compileAndLoad(model.objectName, src)
            catch case e: RuntimeException =>
              throw SpecAssemble.diagnostic(new RuntimeException(
                e.getMessage.replace(model.objectName + ".scala", path), e))
          }
        val methods = loaded.cls.getMethods
          .filter(m => m.getName.matches("action\\d+"))
          .sortBy(m => m.getName.drop("action".length).toInt)
          .toIndexedSeq
        if methods.length != nActions then
          throw SpecAssemble.diagnostic(new RuntimeException(
            s"$path: stale actions object (${methods.length} actions compiled, " +
            s"$nActions in the spec) -- rebuild"))
        methods.map(m => (s: String, p: scup.Pos) =>
          try m.invoke(loaded.instance, s, p).asInstanceOf[Token[K]]
          catch case e: java.lang.reflect.InvocationTargetException =>
            throw SpecAssemble.respec(e.getCause, model.objectName, path))

    // each rule is named by its spec text, for traces and dumps
    var next = 0
    val rules: List[Rule[Token[K]]] = compiledRules.map {
      case (FixedRule(kind, text, _), re) =>
        val k = kindMap(kind)
        Rule(re, Some((s, p) => Token(k, s, p)), s"\"$text\"")
      case (PatRule(pattern, _, Some(_)), re) =>
        val act = actions(next)
        next += 1
        Rule(re, Some((s, p) => act(s, p)), pattern)
      case (PatRule(pattern, _, None), re) =>
        Rule(re, None, pattern)
    }

    val literals: Map[String, K] =
      model.rules.collect { case FixedRule(kind, text, _) => text -> kindMap(kind) }.toMap
    val kindNames: Map[K, String] =
      model.rules.collect { case FixedRule(kind, text, _) => kindMap(kind) -> s"'$text'" }
        .reverse.toMap // first literal for a kind wins

    // build the DFA now, so a runaway spec fails here, named, not at
    // the first scan
    val lexer = new Lexer[Token[K]](rules*)
    try lexer.dfa
    catch case e: IllegalStateException => throw new SpecError(path, 1, e.getMessage)
    new LexSpec[K](lexer, kindMap, literals, kindNames, kindsFqcn, model)

  /** The load-time checks that need no Scala compiler: every %token
   *  row's kind must be an enum member (with a did-you-mean for
   *  typos), and no literal may appear twice.  Enum members with no
   *  row are fine -- their tokens come from action blocks;
   *  `slex.check` lists them on request. */
  private def validate[K](path: String, model: LexSpecModel,
                          kindMap: Map[String, K]): Unit =
    val seenLiterals = scala.collection.mutable.Map[String, Int]()
    for r <- model.rules do r match
      case FixedRule(kind, text, line) =>
        if !kindMap.contains(kind) then
          throw new SpecError(path, line,
            s"'$kind' is not a member of the token-kind enum" +
            scup.GrammarSpec.suggest(kind, kindMap.keys))
        seenLiterals.get(text) match
          case Some(prev) => throw new SpecError(path, line,
            s"duplicate %token literal \"$text\" (first at line $prev)")
          case None => seenLiterals(text) = line
      case _ => ()
