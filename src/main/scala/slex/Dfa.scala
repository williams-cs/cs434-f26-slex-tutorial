package slex

import scala.collection.mutable

/**
 * Subset construction and the table-driven scan -- the rest of the
 * lex pipeline, in the form you saw it in class.
 *
 * `Dfa.compile` turns the combined NFA into a DFA over the ASCII
 * alphabet.  Each DFA state is a set of NFA states (its "members");
 * the start state is the ε-closure of the NFA's start, and the
 * transition on character c from S is the ε-closure of every NFA
 * state reachable from a member of S on c -- exactly the algorithm
 * you ran by hand in HW 1.  Per DFA state we precompute what maximal
 * munch and the trace need: the lowest rule tag among its accepting
 * members (first-rule-wins, folded into the table), every accepting
 * tag, and which rules still have a member alive.
 *
 * The alphabet is ASCII: the table has 128 columns, and the scanner
 * rejects any other character before it reaches either engine.
 */
final class Dfa private (
    val size: Int,
    trans: Array[Int],                 // size * 128; -1 is the dead state
    accept: Array[Int],                // lowest accepting rule tag, -1 if none
    acceptsAll: Array[List[Int]],      // every accepting tag, sorted
    alive: Array[List[Int]],           // rules with a member alive, sorted
    members: Array[List[Int]]):        // the NFA states, sorted (for dumps)


  /**
   * Maximal munch by table walk, starting at offset `from` and reading
   * no further than `limit`.  Same contract as Nfa.longestMatch: the
   * length of the longest match (0 if none), first-listed rule on a
   * tie, zero-length matches ignored.  One array lookup per character.
   */
  private[slex] def longestMatch(src: String, from: Int, limit: Int,
                                 narrate: Narrator): Munch =
    var s        = 0
    var bestLen  = 0
    var bestRule = -1
    var i = from
    while s >= 0 && i < limit do
      val c = src.charAt(i)
      s = trans(s * Dfa.Alphabet + c)
      i += 1
      if s >= 0 then
        if accept(s) >= 0 then { bestLen = i - from; bestRule = accept(s) }
        if narrate != null then narrate.step(c, i - from, alive(s), acceptsAll(s))
      else if narrate != null then narrate.step(c, i - from, Nil, Nil)
    Munch(bestLen, bestRule, i - from, s >= 0)

  /**
   * The machine, printable: every state with its NFA members and
   * accept tags, and its out-edges grouped by target and labeled by
   * the characters that take them.  `label` names the rules, as in
   * the NFA dump.
   */
  def dump(label: Int => String = null): String =
    def lbl(r: Int) = s"#${r + 1}" + (if label == null then "" else s" ${label(r)}")
    val sb = StringBuilder()
    sb ++= s"DFA ($size states; state 0 is the start; each state lists its NFA states):\n"
    for s <- 0 until size do
      sb ++= f"  $s%4d = {${members(s).mkString(", ")}}"
      acceptsAll(s) match
        case Nil      => ()
        case r :: Nil => sb ++= s"  accepts ${lbl(r)}"
        case rs       => sb ++= s"  accepts ${rs.map(lbl).mkString(", ")} -- first listed wins"
      sb ++= "\n"
      val byTarget = mutable.LinkedHashMap[Int, List[Int]]()   // insertion order = first char
      for c <- 0 until Dfa.Alphabet do
        val t = trans(s * Dfa.Alphabet + c)
        if t >= 0 then byTarget(t) = c :: byTarget.getOrElse(t, Nil)
      for (t, csRev) <- byTarget do
        sb ++= f"       --${Dfa.showClass(csRev.reverse)}--> $t%n"
    sb.result()

object Dfa:
  /** The DFA's alphabet: the ASCII characters 0..127. */
  val Alphabet = 128

  /** Subset construction is exponential in the worst case; a course
   *  lexer needs a few hundred states, so anything past this is a
   *  runaway spec, not a big one. */
  val MaxStates = 50000

  /** Subset construction over the ASCII alphabet. */
  def compile(nfa: Nfa): Dfa =
    val index = mutable.HashMap[Set[Int], Int]()
    val sets  = mutable.ArrayBuffer[Set[Int]]()
    val trans = mutable.ArrayBuffer[Int]()
    def intern(s: Set[Int]): Int =
      index.getOrElseUpdate(s, { sets += s; sets.length - 1 })
    intern(nfa.close(Set(nfa.start)))
    var s = 0
    while s < sets.length do                     // sets grows as we go: a worklist
      if sets.length > MaxStates then
        throw new IllegalStateException(
          s"slex: this lexer's DFA exceeds $MaxStates states -- simplify its rules")
      val here = sets(s)
      var c = 0
      while c < Alphabet do
        val t = nfa.close(nfa.step(here, c.toChar))
        trans += (if t.isEmpty then -1 else intern(t))
        c += 1
      s += 1
    val n = sets.length
    val accept     = new Array[Int](n)
    val acceptsAll = new Array[List[Int]](n)
    val alive      = new Array[List[Int]](n)
    val members    = new Array[List[Int]](n)
    for i <- 0 until n do
      val m   = sets(i).toList
      val acc = m.flatMap(nfa.accepts.get).distinct.sorted
      acceptsAll(i) = acc
      accept(i)     = if acc.isEmpty then -1 else acc.head
      alive(i)      = m.map(nfa.owner).filter(_ >= 0).distinct.sorted
      members(i)    = m.sorted
    new Dfa(n, trans.toArray, accept, acceptsAll, alive, members)

  /** A set of ASCII codes as a lex-style edge label: one character as
   *  'c', several as a class with runs collapsed to ranges, [a-z0-9_]. */
  private[slex] def showClass(cs: List[Int]): String =
    def inClass(c: Int): String = c match
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case ']' | '\\' | '-' | '^' => "\\" + c.toChar
      case c if c < 32 || c == 127 => f"\\x$c%02x"
      case c => c.toChar.toString
    cs match
      case c :: Nil => Nfa.showChar(c.toChar)
      case _ =>
        val sb = StringBuilder("[")
        var i = 0
        while i < cs.length do
          var j = i
          while j + 1 < cs.length && cs(j + 1) == cs(j) + 1 do j += 1
          if j - i >= 2 then sb ++= inClass(cs(i)) + "-" + inClass(cs(j))
          else for k <- i to j do sb ++= inClass(cs(k))
          i = j + 1
        sb ++= "]"
        sb.result()
