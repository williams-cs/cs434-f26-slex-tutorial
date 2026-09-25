package slex

/**
 * Thompson construction and NFA simulation -- two of the classic
 * algorithms behind lex, in the form you saw them in class.  (The
 * third, subset construction, is in Dfa.scala.)
 *
 * One NFA runs *all* of a lexer's rules at once: a fresh start state
 * has an ε-edge to each rule's fragment, and each fragment's accept
 * state is tagged with its rule's index.  Simulating the combined
 * machine over the input tells us, at every prefix length, which
 * rules could accept -- which is exactly what maximal munch needs.
 * The DFA built from it answers the same question by table lookup.
 *
 * States are just Ints indexing into edge tables.  Symbol edges carry
 * a predicate (so a whole character class is one edge) plus the
 * pattern text that made it (for dumps and traces), ε-edges carry
 * nothing.  `owner(s)` is the rule whose fragment state s belongs to
 * (-1 for the shared start state), which is how a trace knows which
 * rules are still alive.
 */
final class Nfa private (
    private[slex] val start: Int,
    epsEdges: Array[List[Int]],
    symEdges: Array[List[(String, Char => Boolean, Int)]],
    private[slex] val accepts: Map[Int, Int], // accepting state -> index of its rule
    private[slex] val owner: Array[Int]       // state -> rule index, -1 for the start
):

  /** ε-closure: everything reachable from `states` along ε-edges alone. */
  private[slex] def close(states: Set[Int]): Set[Int] =
    var result   = states
    var frontier = states.toList
    while frontier.nonEmpty do
      val s = frontier.head
      frontier = frontier.tail
      for t <- epsEdges(s) if !result(t) do
        result += t
        frontier ::= t
    result

  /** One step: every state reachable from `states` on character c. */
  private[slex] def step(states: Set[Int], c: Char): Set[Int] =
    states.flatMap(s => symEdges(s).collect { case (_, p, t) if p(c) => t })

  /** How many states the machine has. */
  private[slex] def size: Int = epsEdges.length

  /**
   * Maximal munch by NFA simulation, starting at offset `from` and
   * reading no further than `limit`: run all rules in lockstep,
   * remembering the last point at which some rule accepted.  The
   * result's length is that of the *longest* match (0 if no rule
   * matched even one character); if several rules match that same
   * longest lexeme, the one listed *first* wins.  Zero-length matches
   * are ignored (a rule whose regex matches the empty string would
   * otherwise make no progress).  With a narrator, reports the alive
   * and accepting rules after every character.
   */
  private[slex] def longestMatch(src: String, from: Int, limit: Int,
                                 narrate: Narrator): Munch =
    var states   = close(Set(start))
    var bestLen  = 0
    var bestRule = -1
    var i = from
    while states.nonEmpty && i < limit do
      val c = src.charAt(i)
      states = close(step(states, c))
      i += 1
      val acc = states.flatMap(accepts.get).toList.sorted
      if acc.nonEmpty then { bestLen = i - from; bestRule = acc.head }
      if narrate != null then
        narrate.step(c, i - from, states.map(owner).filter(_ >= 0).toList.distinct.sorted, acc)
    Munch(bestLen, bestRule, i - from, states.nonEmpty)

  /**
   * The machine, printable: every state's ε- and symbol edges (labeled
   * with the pattern text that made them) and each accept state's rule
   * tag.  `label` names the rules, as in `longestMatch`.
   */
  def dump(label: Int => String = null): String =
    def lbl(r: Int) = s"#${r + 1}" + (if label == null then "" else s" ${label(r)}")
    val sb = StringBuilder()
    sb ++= s"NFA (${epsEdges.length} states; state $start is the start," +
           " with an ε-edge to each rule's fragment):\n"
    for s <- epsEdges.indices do
      if epsEdges(s).nonEmpty then
        sb ++= f"  $s%4d --ε--> ${epsEdges(s).sorted.mkString(", ")}%n"
      for (d, _, t) <- symEdges(s).reverse do
        sb ++= f"  $s%4d --$d--> $t%n"
      for r <- accepts.get(s) do
        sb ++= f"  $s%4d accepts ${lbl(r)}%n"
    sb.result()

object Nfa:

  /** Escapes for the characters a trace or dump prints. */
  private[slex] def showChar(c: Char): String = c match
    case '\n' => "'\\n'"
    case '\t' => "'\\t'"
    case '\r' => "'\\r'"
    case c    => s"'$c'"

  private[slex] def showString(s: String): String =
    "\"" + s.flatMap {
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case '"'  => "\\\""
      case c    => c.toString
    } + "\""

  /** Compile the rules' regexes into one combined machine. */
  def compile(res: Seq[Re]): Nfa =
    import scala.collection.mutable.ArrayBuffer
    val epsEdges = ArrayBuffer[List[Int]]()
    val symEdges = ArrayBuffer[List[(String, Char => Boolean, Int)]]()
    val owner    = ArrayBuffer[Int]()
    var building = -1                 // the rule whose fragment we are in

    def newState(): Int =
      epsEdges += Nil
      symEdges += Nil
      owner    += building
      epsEdges.length - 1

    def eps(a: Int, b: Int): Unit = epsEdges(a) ::= b
    def sym(a: Int, d: String, p: Char => Boolean, b: Int): Unit =
      symEdges(a) ::= (d, p, b)

    /**
     * Thompson construction: turn `re` into an NFA fragment with one
     * start and one accept state, connected exactly as in the five
     * pictures from class.  Returns (start, accept).
     */
    def frag(re: Re): (Int, Int) = re match
      case Re.Eps =>
        val s = newState(); val a = newState()
        eps(s, a)
        (s, a)
      case Re.Sym(d, p) =>
        val s = newState(); val a = newState()
        sym(s, d, p, a)
        (s, a)
      case Re.Cat(l, r) =>
        val (ls, la) = frag(l)
        val (rs, ra) = frag(r)
        eps(la, rs)
        (ls, ra)
      case Re.Alt(l, r) =>
        val s = newState(); val a = newState()
        val (ls, la) = frag(l)
        val (rs, ra) = frag(r)
        eps(s, ls); eps(s, rs); eps(la, a); eps(ra, a)
        (s, a)
      case Re.Star(r) =>
        val s = newState(); val a = newState()
        val (rs, ra) = frag(r)
        eps(s, rs); eps(s, a); eps(ra, rs); eps(ra, a)
        (s, a)

    val start = newState()
    val accepts = res.zipWithIndex.map { (re, i) =>
      building = i
      val (s, a) = frag(re)
      eps(start, s)
      a -> i
    }.toMap

    new Nfa(start, epsEdges.toArray, symEdges.toArray, accepts, owner.toArray)
