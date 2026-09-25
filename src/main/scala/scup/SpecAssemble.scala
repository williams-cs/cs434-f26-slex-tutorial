package scup

/**
 * The pure spec-text -> aligned-synthetic-source transformation shared
 * by the two spec-file loaders (slex.LexSpec, scup.GrammarSpec) and by
 * the sbt build-time generator (project/SpecGen.scala, which carries a
 * verbatim copy of this file in each consuming project's metabuild).
 *
 * Every prelude line and action-block line of the generated source
 * appears at exactly its spec-file line number, so compiler
 * diagnostics inside actions carry real spec lines (the #line trick).
 *
 * IMPORTANT: this file is compiled by BOTH Scala 2.12 (the sbt
 * metabuild) and Scala 3 (as library source).  Keep it in the
 * cross-compiling subset: braces syntax, no enums, no extension
 * methods, no 2.13+ stdlib calls.
 */
/**
 * A spec-file problem: message carries path and 1-based line.  The
 * stack trace is trimmed to the user's own frames (the message says
 * everything else), the offending spec line rides underneath in
 * context as the cause, and toString is the bare message, so a
 * surfaced SpecError reads like a compiler diagnostic
 * ("calc.scup:18: ...") rather than an exception.
 */
final class SpecError(val path: String, val line: Int, val msg: String)
    extends Exception(path + ":" + line + ": " + msg) {
  SpecAssemble.diagnostic(this)
  locally {
    val ctx = SpecAssemble.excerpt(path, line)
    if (ctx != null) initCause(new SpecAssemble.SpecExcerpt(ctx))
  }
  override def toString(): String = getMessage
}

object SpecAssemble {

  /** Compatibility alias: SpecError predates its move to the top
   *  level (out of this object, whose nested name leaked into
   *  displays as SpecAssemble$SpecError). */
  type SpecError = scup.SpecError

  /** A `{ ... }` action block; lines/cols are 1-based, cols point at the braces. */
  final case class Block(openLine: Int, openCol: Int, closeLine: Int, closeCol: Int)

  sealed trait SRule { def line: Int }
  /** One `%token` row entry: an ordinary fixed-text rule at its position. */
  final case class FixedRule(kind: String, text: String, line: Int) extends SRule
  /** A `PATTERN [{ block }]` rule; no block = skip rule. */
  final case class PatRule(pattern: String, line: Int, block: Option[Block]) extends SRule

  final case class LexSpecModel(
      path: String,
      lines: Vector[String],
      kinds: Option[String],
      preludeStart: Int, // 1-based first verbatim prelude line; 0 = no prelude
      preludeEnd: Int,   // 1-based last verbatim prelude line
      rules: List[SRule],
      hash: String) {
    def objectName: String = objectNameFor(path)
    /** Block-bearing rules in order; actionN is the Nth one's action. */
    def actionBlocks: List[Block] =
      rules.collect { case PatRule(_, _, Some(b)) => b }
  }

  /** "ic.slex" -> "ic_slex": the generated object's name. */
  def objectNameFor(path: String): String = {
    val norm = path.replace('\\', '/')
    val name = norm.substring(norm.lastIndexOf('/') + 1)
    name.map(c => if (c.isLetterOrDigit) c else '_')
  }

  /** The package holding all generated actions objects. */
  val genPackage = "specgen"

  /**
   * Point an action-block exception back at the spec file: rewrite
   * every stack frame of the generated actions object (objectName
   * + ".scala") to name the spec itself.  The generated source is
   * line-aligned with the spec (see render), so the line numbers
   * carry over unchanged -- a crash inside a `{ ... }` block reads
   * `at specgen.calc_scup$.action1(calc.scup:33)`.  Such traces are
   * also trimmed of library plumbing (the parse/lex engines,
   * reflection, runtime): what remains is the spec line and the
   * user's own frames, which tell the whole story.  Applied down the
   * cause chain; called where the reflective action call is unwrapped.
   */
  def respec(t: Throwable, objectName: String, path: String): Throwable = {
    val gen = objectName + ".scala"
    var c = t
    var depth = 0
    var excerptLine = 0
    while (c != null && depth < 16) {
      val st = c.getStackTrace
      if (st.exists(f => gen.equals(f.getFileName))) {
        val mapped: Array[StackTraceElement] = st.map { f =>
          if (gen.equals(f.getFileName))
            new StackTraceElement(f.getClassName, f.getMethodName, path, f.getLineNumber)
          else f
        }
        val trimmed = mapped.filter(userFrame)
        c.setStackTrace(if (trimmed.isEmpty) mapped else trimmed)
        if (excerptLine == 0) {
          val top = mapped.find(f => path.equals(f.getFileName))
          if (top.isDefined) excerptLine = top.get.getLineNumber
        }
      }
      c = c.getCause
      depth += 1
    }
    if (excerptLine > 0) {
      val ex = excerpt(path, excerptLine)
      if (ex != null) {
        var last: Throwable = t
        var d = 0
        while (last.getCause != null && d < 16) { last = last.getCause; d += 1 }
        if (!last.isInstanceOf[SpecExcerpt]) {
          try last.initCause(new SpecExcerpt(ex))
          catch { case _: Exception => t.addSuppressed(new SpecExcerpt(ex)) }
        }
      }
    }
    t
  }

  /** The spec excerpt shown beneath a trace.  Riding as the terminal
   *  cause, it follows the exception without changing its type or
   *  message and prints as `Caused by: <path>:<line>: <lines>`; the
   *  emptied stack keeps printers from adding frames under it. */
  final class SpecExcerpt(msg: String) extends Exception(msg) {
    setStackTrace(new Array[StackTraceElement](0))
    override def toString(): String = getMessage
  }

  /** Trim a diagnostic's stack trace to the user's own frames: for an
   *  error whose message says everything (a spec problem, a conflict,
   *  a parse error), the library frames beneath it are noise.  Left
   *  alone if no user frame would remain. */
  def diagnostic[T <: Throwable](t: T): T = {
    val trimmed = t.getStackTrace.filter(userFrame)
    if (trimmed.nonEmpty) t.setStackTrace(trimmed)
    t
  }

  /** path:line plus two spec lines of context either side, the crash
   *  line marked; null if the file cannot be read (never fails). */
  private[scup] def excerpt(path: String, line: Int): String = {
    try {
      val all = java.nio.file.Files.readAllLines(
        java.nio.file.Paths.get(path), java.nio.charset.StandardCharsets.UTF_8)
      if (line < 1 || line > all.size) null
      else {
        val lo = math.max(1, line - 2)
        val hi = math.min(all.size, line + 2)
        val sb = new StringBuilder
        sb.append(path).append(":").append(line).append(":")
        var i = lo
        while (i <= hi) {
          sb.append("\n").append(if (i == line) " -> " else "    ")
          sb.append(i).append(" | ").append(all.get(i - 1))
          i += 1
        }
        sb.toString
      }
    } catch { case _: Exception => null }
  }

  /** Frames a spec author can act on: their action blocks (package
   *  specgen, remapped to the spec file above) and their own code.
   *  Everything else in a spec-related trace is plumbing. */
  def userFrame(f: StackTraceElement): Boolean = {
    val c = f.getClassName
    !(c.startsWith("scup.") || c.startsWith("slex.") ||
      c.startsWith("java.") || c.startsWith("jdk.") || c.startsWith("sun.") ||
      c.startsWith("scala."))
  }

  def sha256(text: String): String = {
    val md = java.security.MessageDigest.getInstance("SHA-256")
    val bytes = md.digest(text.getBytes("UTF-8"))
    val sb = new StringBuilder
    var i = 0
    while (i < bytes.length) {
      sb.append("%02x".format(bytes(i) & 0xff))
      i += 1
    }
    sb.toString
  }

  /*----------------------- Scala-aware brace matching -----------------------*/

  /**
   * Find the index of the '}' matching the '{' at `open`, skipping
   * braces inside Scala line comments, (nested) block comments,
   * strings (single- and triple-quoted, including `${...}`
   * interpolation regions, which recursively contain code), and
   * character literals.  Returns -1 if unbalanced.
   */
  def matchBrace(text: String, open: Int): Int = {
    var i = open
    var depth = 0
    val n = text.length
    while (i < n) {
      val c = text.charAt(i)
      if (c == '/' && i + 1 < n && text.charAt(i + 1) == '/') {
        while (i < n && text.charAt(i) != '\n') i += 1
      } else if (c == '/' && i + 1 < n && text.charAt(i + 1) == '*') {
        var cdepth = 1
        i += 2
        while (i < n && cdepth > 0) {
          if (text.charAt(i) == '/' && i + 1 < n && text.charAt(i + 1) == '*') { cdepth += 1; i += 2 }
          else if (text.charAt(i) == '*' && i + 1 < n && text.charAt(i + 1) == '/') { cdepth -= 1; i += 2 }
          else i += 1
        }
      } else if (c == '"') {
        i = skipString(text, i)
      } else if (c == '\'' && i + 2 < n &&
                 (text.charAt(i + 1) != '\\' && text.charAt(i + 2) == '\'')) {
        i += 3 // simple char literal 'x'
      } else if (c == '\'' && i + 1 < n && text.charAt(i + 1) == '\\') {
        var j = i + 2
        while (j < n && text.charAt(j) != '\'') j += 1
        i = j + 1
      } else if (c == '{') {
        depth += 1
        i += 1
      } else if (c == '}') {
        depth -= 1
        if (depth == 0) return i
        i += 1
      } else {
        i += 1
      }
    }
    -1
  }

  /** Skip a string starting at the '"' at `start`; returns the index
   *  after it.  Handles triple quotes and `${...}` interpolations. */
  private def skipString(text: String, start: Int): Int = {
    val n = text.length
    if (start + 2 < n && text.charAt(start + 1) == '"' && text.charAt(start + 2) == '"') {
      var i = start + 3
      while (i + 2 < n &&
             !(text.charAt(i) == '"' && text.charAt(i + 1) == '"' && text.charAt(i + 2) == '"'))
        i = skipInterp(text, i)
      if (i + 2 < n) i + 3 else n
    } else {
      var i = start + 1
      var done = false
      while (i < n && !done) {
        val c = text.charAt(i)
        if (c == '\\' && i + 1 < n) i += 2
        else if (c == '"') { done = true; i += 1 }
        else if (c == '\n') { done = true } // unterminated; bail at line end
        else i = skipInterp(text, i)
      }
      i
    }
  }

  /** At `${` inside a string, skip the whole interpolation region
   *  (code, possibly with nested braces/strings); else advance one. */
  private def skipInterp(text: String, i: Int): Int = {
    if (text.charAt(i) == '$' && i + 1 < text.length && text.charAt(i + 1) == '{') {
      val close = matchBrace(text, i + 1)
      if (close < 0) text.length else close + 1
    } else i + 1
  }

  /*------------------------------ parsing ---------------------------------*/

  private def isIdentStart(c: Char) = c.isLetter || c == '_'
  private def isIdentChar(c: Char) = c.isLetterOrDigit || c == '_'

  /** Parse `KIND "literal"` pairs filling a whole line (with optional
   *  trailing # comment).  None = the line is not a %token row. */
  private[scup] def parseTokensRow(line: String): Option[List[(String, String)]] = {
    val pairs = scala.collection.mutable.ListBuffer[(String, String)]()
    var i = 0
    val n = line.length
    def skipWs(): Unit = { while (i < n && line.charAt(i).isWhitespace) i += 1 }
    skipWs()
    if (i >= n || !isIdentStart(line.charAt(i))) return None
    var ok = true
    while (ok && i < n && line.charAt(i) != '#') {
      if (!isIdentStart(line.charAt(i))) { ok = false }
      else {
        val ks = i
        while (i < n && isIdentChar(line.charAt(i))) i += 1
        val kind = line.substring(ks, i)
        skipWs()
        if (i >= n || line.charAt(i) != '"') { ok = false }
        else {
          i += 1
          val sb = new StringBuilder
          var closed = false
          while (i < n && !closed) {
            val c = line.charAt(i)
            if (c == '\\' && i + 1 < n) { sb.append(line.charAt(i + 1)); i += 2 }
            else if (c == '"') { closed = true; i += 1 }
            else { sb.append(c); i += 1 }
          }
          if (!closed) ok = false
          else {
            pairs += ((kind, sb.toString))
            skipWs()
          }
        }
      }
    }
    if (ok && pairs.nonEmpty) Some(pairs.toList) else None
  }

  def parseLex(path: String, rawText: String): LexSpecModel = {
    val text = rawText.replace("\r\n", "\n").replace("\r", "\n")
    val lines: Vector[String] = {
      val v = text.split("\n", -1).toVector
      if (v.nonEmpty && v.last.isEmpty) v.init else v
    }
    val nLines = lines.length
    val lineStart = new Array[Int](nLines + 2)
    var off = 0
    var k = 0
    while (k < nLines) { lineStart(k + 1) = off; off += lines(k).length + 1; k += 1 }
    lineStart(nLines + 1) = text.length

    var kinds: Option[String] = None
    var preludeStart = 0
    var preludeEnd = 0
    val rules = scala.collection.mutable.ListBuffer[SRule]()

    def err(line: Int, msg: String): Nothing = throw new SpecError(path, line, msg)

    /** Extract the block whose '{' is at 1-based line ln, 0-based col c.
     *  Returns the Block and the 1-based line to continue at. */
    def readBlock(ln: Int, c: Int): (Block, Int) = {
      val open = lineStart(ln) + c
      val close = matchBrace(text, open)
      if (close < 0) err(ln, "unbalanced '{' in action block")
      var cl = ln
      while (cl < nLines && lineStart(cl + 1) <= close) cl += 1
      val ccol = close - lineStart(cl)
      val after = lines(cl - 1).substring(ccol + 1).trim
      if (after.nonEmpty && !after.startsWith("#"))
        err(cl, "unexpected text after action block: '" + after + "'")
      (Block(ln, c + 1, cl, ccol + 1), cl + 1)
    }

    var li = 1
    while (li <= nLines) {
      val raw = lines(li - 1)
      val t = raw.trim
      if (t.isEmpty || t.startsWith("#")) {
        li += 1
      } else if (t == "%{") {
        if (preludeStart != 0) err(li, "duplicate %{ prelude")
        preludeStart = li + 1
        var j = li + 1
        while (j <= nLines && lines(j - 1).trim != "%}") j += 1
        if (j > nLines) err(li, "%{ with no closing %}")
        preludeEnd = j - 1
        li = j + 1
      } else if (t == "%}") {
        err(li, "%} with no opening %{")
      } else if (t.startsWith("%kinds")) {
        val v = t.substring("%kinds".length).trim
        if (v.isEmpty) err(li, "%kinds needs a class name")
        if (kinds.isDefined) err(li, "duplicate %kinds")
        kinds = Some(v)
        li += 1
      } else if (t.startsWith("%token")) {
        val inline0 = t.substring("%token".length)
        if (inline0.trim.nonEmpty) {
          parseTokensRow(inline0) match {
            case Some(ps) => for (p <- ps) rules += FixedRule(p._1, p._2, li)
            case None     => err(li, "bad %token row (rows pair a KIND " +
              "with a quoted literal: KIND \"text\")")
          }
        }
        li += 1
        var stop = false
        while (li <= nLines && !stop) {
          val rt = lines(li - 1).trim
          if (rt.isEmpty || rt.startsWith("#")) li += 1
          else parseTokensRow(lines(li - 1)) match {
            case Some(ps) => for (p <- ps) rules += FixedRule(p._1, p._2, li); li += 1
            case None     => stop = true
          }
        }
      } else if (t.startsWith("%")) {
        err(li, "unknown declaration '" + t.split("\\s+")(0) +
          "' (a pattern that starts with a literal '%' must escape it: \\%)")
      } else if (t.startsWith("{")) {
        // an action on its own line belongs to the preceding pattern
        val c = raw.indexOf('{')
        rules.lastOption match {
          case Some(PatRule(p, pl, None)) =>
            val (b, next) = readBlock(li, c)
            rules.remove(rules.length - 1)
            rules += PatRule(p, pl, Some(b))
            li = next
          case _ => err(li, "action block with no preceding pattern")
        }
      } else {
        // a rule line: PATTERN [ { block } ]
        var j = 0
        while (j < raw.length && raw.charAt(j).isWhitespace) j += 1
        val ps = j
        var done = false
        var inClass = false // whitespace inside [...] does not end the pattern
        while (j < raw.length && !done) {
          val c = raw.charAt(j)
          if (c == '\\' && j + 1 < raw.length) j += 2
          else if (inClass) { if (c == ']') inClass = false; j += 1 }
          else if (c == '[') { inClass = true; j += 1 }
          else if (c.isWhitespace) done = true
          else j += 1
        }
        val pattern = raw.substring(ps, j)
        while (j < raw.length && raw.charAt(j).isWhitespace) j += 1
        if (j >= raw.length || raw.charAt(j) == '#') {
          rules += PatRule(pattern, li, None)
          li += 1
        } else if (raw.charAt(j) == '{') {
          val (b, next) = readBlock(li, j)
          rules += PatRule(pattern, li, Some(b))
          li = next
        } else {
          val junk = raw.substring(j)
          val hint =
            if (junk.startsWith("\""))
              " -- fixed-text tokens are declared in a %token table, not as rules"
            else if (pattern.nonEmpty && pattern.forall(ch => ch.isUpper || ch.isDigit || ch == '_'))
              " -- if this was meant as a %token row, the literal needs " +
              "double quotes: KIND \"text\""
            else ""
          err(li, "expected '{' or end of line after pattern, got '" +
            junk + "'" + hint)
        }
      }
    }

    new LexSpecModel(path, lines, kinds, preludeStart, preludeEnd,
      rules.toList, sha256(text))
  }

  /*----------------------------- assembly ---------------------------------*/

  /**
   * The shared aligned-emission core: prelude lines and action-block
   * lines land at exactly their spec line numbers; each action's
   * scaffold (its `def` signature) shares its block's opening-brace
   * line; the object header shares line 1; the footer trails the last
   * spec line.
   */
  private def render(lines: Vector[String], preludeStart: Int, preludeEnd: Int,
                     objectName: String, hash: String, kindsFqcn: String,
                     actions: List[(Block, String)]): String = {
    val out = Array.fill(if (lines.isEmpty) 1 else lines.length)("")
    if (preludeStart > 0) {
      var i = preludeStart
      while (i <= preludeEnd) { out(i - 1) = lines(i - 1); i += 1 }
    }
    // Match the prelude's indentation so the object body's statements
    // share one indentation base (else Scala 3 warns "indented too far
    // to the left" on the generated defs).
    val indent =
      if (preludeStart == 0) ""
      else {
        val first = (preludeStart to preludeEnd)
          .map(i => lines(i - 1)).find(_.trim.nonEmpty)
        first match {
          case Some(l) => l.substring(0, l.indexWhere(!_.isWhitespace))
          case None    => ""
        }
      }
    // Two actions may share a line (e.g. `a { f } | b { g }`): join
    // segments landing on an occupied line with `;`.
    def emit(line: Int, seg: String): Unit = {
      out(line - 1) =
        if (out(line - 1).isEmpty) seg
        else out(line - 1) + " ; " + seg.trim
    }
    for (ba <- actions) {
      val b = ba._1
      val scaffold = indent + ba._2
      val openLineText = lines(b.openLine - 1)
      if (b.openLine == b.closeLine) {
        emit(b.openLine, scaffold +
          openLineText.substring(b.openCol, b.closeCol - 1) + "}")
      } else {
        emit(b.openLine, scaffold + openLineText.substring(b.openCol))
        var i = b.openLine + 1
        while (i < b.closeLine) { out(i - 1) = lines(i - 1); i += 1 }
        emit(b.closeLine, lines(b.closeLine - 1).substring(0, b.closeCol))
      }
    }
    // Token and the kind enum's members are in scope in every block
    // (the spec names the enum; requiring the import would be noise).
    val header = "package " + genPackage + " { object " + objectName +
      " { val specHash = \"" + hash + "\"; import scup.Token; " +
      "import " + kindsFqcn + ".*; "
    out(0) = header + out(0)
    out.mkString("\n") + "\n} }\n"
  }

  /**
   * The aligned synthetic actions object for a lexer spec.  Each
   * actionN has signature (text: String, pos: Pos) => Token[K], with a
   * local `error(msg): Nothing` helper in scope.
   */
  def assembleLex(m: LexSpecModel, kindsFqcn: String): String = {
    val tokenType = "scup.Token[" + kindsFqcn + "]"
    var n = -1
    val actions = m.rules.flatMap {
      case PatRule(_, _, Some(b)) =>
        n += 1
        List((b,
          "def action" + n + "(text: String, pos: scup.Pos): " + tokenType +
          " = { def error(msg: String): Nothing = { throw slex.LexError(msg, pos) }; "))
      case _ => Nil
    }
    render(m.lines, m.preludeStart, m.preludeEnd, m.objectName, m.hash,
      kindsFqcn, actions)
  }

  /*========================== .scup grammar specs ==========================*/

  /** EBNF modifiers on a symbol reference. */
  sealed trait Mod
  case object MOpt extends Mod                       // x?   -> Option
  case object MStar extends Mod                      // x*   -> List
  case object MPlus extends Mod                      // x+   -> List
  final case class MStarSep(sep: SymRef) extends Mod // %list(x, sep) -> List

  /** A symbol reference in a production. */
  sealed trait SymRef { def line: Int; def mods: List[Mod] }
  final case class SName(name: String, mods: List[Mod], line: Int) extends SymRef
  final case class SLit(text: String, mods: List[Mod], line: Int) extends SymRef
  /** `("a" | "b")`: an alternation of terminals only. */
  final case class SOneOf(alts: List[SymRef], mods: List[Mod], line: Int) extends SymRef

  /** One alternative of a rule. */
  sealed trait AltDef { def line: Int; def block: Option[Block] }
  final case class AEmpty(block: Option[Block], line: Int) extends AltDef
  final case class ASyms(syms: List[SymRef], block: Option[Block], line: Int) extends AltDef
  final case class PrecLevel(rightAssoc: Boolean, elems: List[SymRef], line: Int)
  final case class ABinary(operand: String, levels: List[PrecLevel],
                           block: Option[Block], line: Int) extends AltDef
  final case class AChain(base: SymRef, suffix: String, line: Int) extends AltDef {
    def block: Option[Block] = None
  }

  final case class RuleDef(name: String, line: Int, greedy: Boolean, alts: List[AltDef])
  final case class TypeDecl(tpe: String, line: Int)

  final case class ScupSpecModel(
      path: String,
      lines: Vector[String],
      preludeStart: Int,
      preludeEnd: Int,
      start: Option[String],
      types: Map[String, TypeDecl],
      rules: List[RuleDef],
      hash: String) {
    def objectName: String = objectNameFor(path)
    def ruleNames: Set[String] = rules.map(_.name).toSet
    /** Rules used as the suffix of a %chain (their actions take $$). */
    def chainSuffixes: Set[String] =
      rules.flatMap(_.alts).collect { case AChain(_, s, _) => s }.toSet
    /** suffix-rule name -> the chain rule that uses it. */
    def chainOwner: Map[String, String] =
      (for (r <- rules; a <- r.alts) yield a match {
        case AChain(_, s, _) => List((s, r.name))
        case _               => Nil
      }).flatten.toMap
    /** A rule with no blocks and no %type parses to generic STree nodes. */
    def isBare(r: RuleDef): Boolean =
      r.alts.forall(_.block.isEmpty) && !types.contains(r.name) &&
        !r.alts.exists(a => a.isInstanceOf[ABinary] || a.isInstanceOf[AChain])
  }

  /*--------------------------- .scup tokenizing ---------------------------*/

  private final case class SpecTok(kind: String, text: String, line: Int, block: Block)

  private def splitLines(rawText: String): (String, Vector[String], Array[Int]) = {
    val text = rawText.replace("\r\n", "\n").replace("\r", "\n")
    val lines: Vector[String] = {
      val v = text.split("\n", -1).toVector
      if (v.nonEmpty && v.last.isEmpty) v.init else v
    }
    val lineStart = new Array[Int](lines.length + 2)
    var off = 0
    var k = 0
    while (k < lines.length) { lineStart(k + 1) = off; off += lines(k).length + 1; k += 1 }
    lineStart(lines.length + 1) = text.length
    (text, lines, lineStart)
  }

  private def scupTokens(path: String, text: String, lines: Vector[String],
                         lineStart: Array[Int]): (List[SpecTok], Int, Int) = {
    val toks = scala.collection.mutable.ListBuffer[SpecTok]()
    var preludeStart = 0
    var preludeEnd = 0
    val n = text.length
    var i = 0
    var line = 1
    def err(l: Int, m: String): Nothing = throw new SpecError(path, l, m)
    def advTo(j: Int): Unit = { while (i < j) { if (text.charAt(i) == '\n') line += 1; i += 1 } }
    while (i < n) {
      val c = text.charAt(i)
      if (c == '\n') { line += 1; i += 1 }
      else if (c.isWhitespace) i += 1
      else if (c == '#') { while (i < n && text.charAt(i) != '\n') i += 1 }
      else if (c == '%' && i + 1 < n && text.charAt(i + 1) == '{') {
        if (preludeStart != 0) err(line, "duplicate %{ prelude")
        preludeStart = line + 1
        var j = line + 1
        while (j <= lines.length && lines(j - 1).trim != "%}") j += 1
        if (j > lines.length) err(line, "%{ with no closing %}")
        preludeEnd = j - 1
        advTo(lineStart(j + 1))
      }
      else if (c == '%') {
        var j = i + 1
        while (j < n && text.charAt(j).isLetter) j += 1
        val word = text.substring(i, j)
        val l0 = line
        if (word == "%start" || word == "%left" || word == "%right" ||
            word == "%greedy" || word == "%empty" || word == "%binary" ||
            word == "%chain" || word == "%list") {
          toks += SpecTok(word, word, l0, null)
          advTo(j)
        } else if (word == "%type") {
          toks += SpecTok(word, word, l0, null)
          advTo(j)
          while (i < n && (text.charAt(i) == ' ' || text.charAt(i) == '\t')) i += 1
          val is = i
          while (i < n && (text.charAt(i).isLetterOrDigit || text.charAt(i) == '_')) i += 1
          if (i == is) err(l0, "%type needs a rule name")
          toks += SpecTok("ident", text.substring(is, i), l0, null)
          while (i < n && (text.charAt(i) == ' ' || text.charAt(i) == '\t')) i += 1
          if (i >= n || text.charAt(i) != ':') err(l0, "%type needs ':' after the rule name")
          i += 1
          var eol = i
          while (eol < n && text.charAt(eol) != '\n') eol += 1
          var t0 = text.substring(i, eol)
          val hashAt = t0.indexOf('#')
          if (hashAt >= 0) t0 = t0.substring(0, hashAt)
          t0 = t0.trim
          if (t0.isEmpty) err(l0, "%type needs a type")
          toks += SpecTok("typetext", t0, l0, null)
          i = eol
        } else err(l0, "unknown declaration '" + word + "'")
      }
      else if (c.isLetter || c == '_') {
        val is = i
        var j = i
        while (j < n && (text.charAt(j).isLetterOrDigit || text.charAt(j) == '_')) j += 1
        toks += SpecTok("ident", text.substring(is, j), line, null)
        i = j
      }
      else if (c == '"') {
        val l0 = line
        val sb = new StringBuilder
        var j = i + 1
        var closed = false
        while (j < n && !closed) {
          val ch = text.charAt(j)
          if (ch == '\\' && j + 1 < n) { sb.append(text.charAt(j + 1)); j += 2 }
          else if (ch == '"') { closed = true; j += 1 }
          else if (ch == '\n') err(l0, "unterminated \"literal\"")
          else { sb.append(ch); j += 1 }
        }
        if (!closed) err(l0, "unterminated \"literal\"")
        toks += SpecTok("lit", sb.toString, l0, null)
        i = j
      }
      else if (c == ':' && i + 2 < n && text.charAt(i + 1) == ':' && text.charAt(i + 2) == '=') {
        toks += SpecTok("::=", "::=", line, null)
        i += 3
      }
      else if (c == '{') {
        val l0 = line
        val close = matchBrace(text, i)
        if (close < 0) err(l0, "unbalanced '{' in action block")
        var nl = 0
        var k = i
        while (k < close) { if (text.charAt(k) == '\n') nl += 1; k += 1 }
        val cl = l0 + nl
        val openCol = i - lineStart(l0) + 1
        val closeCol = close - lineStart(cl) + 1
        toks += SpecTok("block", "{...}", l0, Block(l0, openCol, cl, closeCol))
        advTo(close + 1)
      }
      else if (c == '*' && i + 1 < n && text.charAt(i + 1) == '(') {
        toks += SpecTok("*(", "*(", line, null)
        i += 2
      }
      else if ("|;?*+(),".indexOf(c.toInt) >= 0) {
        toks += SpecTok(c.toString, c.toString, line, null)
        i += 1
      }
      else if (c == '\'')
        err(line, "unexpected ' -- terminal literals use double quotes: \"+\"")
      else if (c == '=')
        err(line, "unexpected '=' -- rules are written  name ::= ... ;")
      else err(line, "unexpected character '" + c + "'")
    }
    (toks.toList, preludeStart, preludeEnd)
  }

  /*----------------------------- .scup parsing ----------------------------*/

  def parseScup(path: String, rawText: String): ScupSpecModel = {
    val (text, lines, lineStart) = splitLines(rawText)
    val (tokList, preludeStart, preludeEnd) = scupTokens(path, text, lines, lineStart)
    val toks = tokList.toIndexedSeq
    var p = 0
    def err(l: Int, m: String): Nothing = throw new SpecError(path, l, m)
    def cur: SpecTok =
      if (p < toks.length) toks(p)
      else SpecTok("eof", "end of file", if (lines.isEmpty) 1 else lines.length, null)
    def at(k: String): Boolean = cur.kind == k
    def eat(k: String, what: String): SpecTok = {
      if (!at(k)) err(cur.line, "expected " + what + ", got '" + cur.text + "'")
      val t = cur
      p += 1
      t
    }

    var start: Option[String] = None
    val types = scala.collection.mutable.LinkedHashMap[String, TypeDecl]()
    val rules = scala.collection.mutable.ListBuffer[RuleDef]()
    var pendingGreedy = false
    var greedyLine = 0
    val pendingLevels = scala.collection.mutable.ListBuffer[PrecLevel]()

    def parsePrimary(): SymRef = cur.kind match {
      case "ident" =>
        val t = eat("ident", "a symbol")
        SName(t.text, Nil, t.line)
      case "lit" =>
        val t = eat("lit", "a symbol")
        SLit(t.text, Nil, t.line)
      case "(" =>
        val t = eat("(", "a symbol")
        val alts = scala.collection.mutable.ListBuffer[SymRef](parseTerminalRef())
        while (at("|")) { p += 1; alts += parseTerminalRef() }
        eat(")", "')'")
        SOneOf(alts.toList, Nil, t.line)
      case "%list" =>
        // %list(x, sep): a sep-separated, possibly empty list of x.
        eat("%list", "%list")
        eat("(", "'('")
        val elem = parsePrimary()
        eat(",", "','")
        val sep = parsePrimary()
        eat(")", "')' after the separator")
        withMod(elem, MStarSep(sep))
      case _ => err(cur.line, "expected a symbol, got '" + cur.text + "'")
    }

    def parseTerminalRef(): SymRef = cur.kind match {
      case "ident" => val t = eat("ident", "a terminal"); SName(t.text, Nil, t.line)
      case "lit"   => val t = eat("lit", "a terminal"); SLit(t.text, Nil, t.line)
      case _       => err(cur.line, "a (...) group may contain only terminals")
    }

    def withMod(s: SymRef, m: Mod): SymRef = s match {
      case SName(nm, ms, l)  => SName(nm, ms :+ m, l)
      case SLit(tx, ms, l)   => SLit(tx, ms :+ m, l)
      case SOneOf(as, ms, l) => SOneOf(as, ms :+ m, l)
    }

    def parseSymbol(): SymRef = {
      var s = parsePrimary()
      var done = false
      while (!done) cur.kind match {
        case "?"  => p += 1; s = withMod(s, MOpt)
        case "*"  => p += 1; s = withMod(s, MStar)
        case "+"  => p += 1; s = withMod(s, MPlus)
        case "*(" =>
          err(cur.line, "x*(sep) is no longer notation; write %list(x, sep)")
        case _ => done = true
      }
      s
    }

    def isSymStart: Boolean = at("ident") || at("lit") || at("(") || at("%list")

    def optBlock(): Option[Block] =
      if (at("block")) { val t = cur; p += 1; Some(t.block) } else None

    def parseAlt(): AltDef = cur.kind match {
      case "%empty" =>
        val t = eat("%empty", "%empty")
        AEmpty(optBlock(), t.line)
      case "%binary" =>
        val t = eat("%binary", "%binary")
        eat("(", "'('")
        val op = eat("ident", "the operand rule")
        eat(")", "')'")
        val b = optBlock()
        if (pendingLevels.isEmpty)
          err(t.line, "%binary with no preceding %left/%right block")
        val lv = pendingLevels.toList
        pendingLevels.clear()
        ABinary(op.text, lv, b, t.line)
      case "%chain" =>
        val t = eat("%chain", "%chain")
        eat("(", "'('")
        val base = parsePrimary()
        eat(",", "','")
        val suf = eat("ident", "the suffix rule")
        eat(")", "')'")
        AChain(base, suf.text, t.line)
      case _ =>
        val l0 = cur.line
        val syms = scala.collection.mutable.ListBuffer[SymRef](parseSymbol())
        while (isSymStart) syms += parseSymbol()
        ASyms(syms.toList, optBlock(), l0)
    }

    while (!at("eof")) cur.kind match {
      case "%start" =>
        val t = eat("%start", "%start")
        if (start.isDefined) err(t.line, "duplicate %start")
        start = Some(eat("ident", "the start rule").text)
      case "%type" =>
        p += 1
        val id = eat("ident", "a rule name")
        val tt = eat("typetext", "a type")
        if (types.contains(id.text)) err(id.line, "duplicate %type for '" + id.text + "'")
        types(id.text) = TypeDecl(tt.text, id.line)
      case "%greedy" =>
        val t = eat("%greedy", "%greedy")
        pendingGreedy = true
        greedyLine = t.line
      case "%left" | "%right" =>
        val t = cur
        p += 1
        // a level's terminals end with its line, yacc-style
        val elems = scala.collection.mutable.ListBuffer[SymRef]()
        while ((at("ident") || at("lit")) && cur.line == t.line)
          elems += parseTerminalRef()
        if (elems.isEmpty) err(t.line, t.kind + " needs at least one terminal")
        pendingLevels += PrecLevel(t.kind == "%right", elems.toList, t.line)
      case "ident" =>
        val name = eat("ident", "a rule name")
        eat("::=", "'::='")
        val alts = scala.collection.mutable.ListBuffer[AltDef](parseAlt())
        while (at("|")) { p += 1; alts += parseAlt() }
        if (at("::="))
          err(cur.line, "found '::=' where ';' was expected -- is a ';' " +
            "missing at the end of the previous rule?")
        eat(";", "';' at the end of the rule")
        if (rules.exists(_.name == name.text))
          err(name.line, "duplicate rule '" + name.text + "'")
        rules += RuleDef(name.text, name.line, pendingGreedy, alts.toList)
        pendingGreedy = false
      case _ =>
        err(cur.line, "expected a declaration or rule, got '" + cur.text + "'")
    }
    if (pendingGreedy) err(greedyLine, "%greedy with no following rule")
    if (pendingLevels.nonEmpty)
      err(pendingLevels.head.line, "%left/%right with no following %binary rule")
    if (rules.isEmpty) err(1, "a grammar needs at least one rule")

    new ScupSpecModel(path, lines, preludeStart, preludeEnd, start,
      types.toMap, rules.toList, sha256(text))
  }

  /*---------------------------- .scup assembly ----------------------------*/

  /**
   * The aligned synthetic actions object for a grammar spec.  Each
   * production's block becomes `def actionN($1: T1, ..., $n: Tn): R`
   * -- `$i` are ordinary parameters, 1-based over ALL symbols of the
   * production, so blocks reference them verbatim.  A rule used as a
   * %chain suffix gets `$$` (the accumulated value) as an extra first
   * parameter.  Types come from %type declarations (Any otherwise),
   * so the Scala compiler statically checks every action against the
   * grammar, with errors at spec lines.
   */
  def assembleScup(m: ScupSpecModel, kindsFqcn: String): String = {
    val tokenType = "scup.Token[" + kindsFqcn + "]"
    def typeOfRule(name: String): String =
      m.types.get(name).map(_.tpe).getOrElse("Any")
    def typeOfSym(s: SymRef): String = {
      var t = s match {
        case SName(nm, _, _) =>
          if (m.ruleNames.contains(nm)) typeOfRule(nm) else tokenType
        case _ => tokenType
      }
      for (mod <- s.mods) t = mod match {
        case MOpt => "Option[" + t + "]"
        case _    => "List[" + t + "]"
      }
      t
    }
    val chainOwner = m.chainOwner
    var n = -1
    val actions = scala.collection.mutable.ListBuffer[(Block, String)]()
    for (r <- m.rules) {
      val isSuffix = m.chainSuffixes.contains(r.name)
      val accType =
        if (!isSuffix) ""
        else m.types.get(r.name).map(_.tpe)
          .orElse(chainOwner.get(r.name).flatMap(o => m.types.get(o)).map(_.tpe))
          .getOrElse("Any")
      for (a <- r.alts) a.block match {
        case Some(b) =>
          n += 1
          val params = scala.collection.mutable.ListBuffer[String]()
          if (isSuffix) params += "$$: " + accType
          a match {
            case ASyms(syms, _, _) =>
              var i = 0
              for (s <- syms) { i += 1; params += "$" + i + ": " + typeOfSym(s) }
            case ABinary(opnd, _, _, _) =>
              val t = if (m.types.contains(r.name)) typeOfRule(r.name)
                      else typeOfRule(opnd)
              params += "$1: " + t
              params += "$2: " + tokenType
              params += "$3: " + t
            case _ => () // AEmpty: no params
          }
          val retType =
            if (isSuffix) accType
            else a match {
              case ABinary(opnd, _, _, _) =>
                if (m.types.contains(r.name)) typeOfRule(r.name) else typeOfRule(opnd)
              case _ => typeOfRule(r.name)
            }
          actions += ((b,
            "def action" + n + "(" + params.mkString(", ") + "): " + retType + " = { "))
        case None => ()
      }
    }
    render(m.lines, m.preludeStart, m.preludeEnd, m.objectName, m.hash,
      kindsFqcn, actions.toList)
  }
}
