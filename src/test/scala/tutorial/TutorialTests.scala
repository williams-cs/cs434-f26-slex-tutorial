package tutorial

import scup.{Pos, Token}
import slex.{LexError, LexSpec}

/**
 * The slex tutorial's claims, as tests over the real spec files at
 * the root of this repository -- the same files the runnable demos
 * use.  If an edit to a spec, a host file, or the vendored library
 * breaks a tutorial example, this suite fails.
 */
class TutorialTests extends munit.FunSuite:

  def kinds[K](ts: IndexedSeq[Token[K]]): List[String] =
    ts.map(t => s"${t.kind}:${t.text}").toList

  // --- The Idea: the toy while-language ------------------------------------

  val toySpec = LexSpec.load[toy.K]("toy.slex")

  test("the toy while-language lexer") {
    assertEquals(kinds(toySpec.tokenize("while (2 <= 10)")),
      List("WHILE:while", "LP:(", "NUM:2", "LEQ:<=", "NUM:10", "RP:)"))
    // the block stored the parsed Int in the token's value field
    assertEquals(toySpec.tokenize("42").head.value, 42)
  }

  // --- The Two Laws --------------------------------------------------------

  test("Law 1: maximal munch, regardless of listing order") {
    val rel = LexSpec.load[laws.RelK]("rel.slex")
    assertEquals(kinds(rel.tokenize("<=<")), List("LEQ:<=", "LT:<"))
  }

  test("Law 2: keyword rows above the identifier rule win ties") {
    val kw = LexSpec.load[laws.KwK]("kw.slex")
    assertEquals(kinds(kw.tokenize("while")), List("WHILE:while"))
    // munch beats priority: the identifier rule matches MORE characters
    assertEquals(kinds(kw.tokenize("whilewhilst")), List("ID:whilewhilst"))
  }

  // --- Watching It Scan ----------------------------------------------------

  test("the trace narrates both laws on whilewhilst") {
    val kw = LexSpec.load[laws.KwK]("kw.slex")
    val lines = List.newBuilder[String]
    kw.tokenize("whilewhilst", trace = lines.addOne)
    val t = lines.result().mkString("\n")
    assert(t.contains("Rules (listing order breaks ties):"))
    // Law 2, at five characters: both rules accept, the first row wins
    assert(t.contains("accept: #1 \"while\", #2 [a-z]+ -- " +
      "first listed wins: #1 \"while\"  (best is now 5 chars)"))
    // Law 1, at the end: the longest accept takes the munch
    assert(t.contains("longest match: #2 [a-z]+ takes 11 chars"))
    assert(t.contains("=> [ID,whilewhilst,1]"))
    // the same scan through the NFA simulation narrates identically
    val nfaLines = List.newBuilder[String]
    kw.tokenize("whilewhilst", trace = nfaLines.addOne, engine = slex.Engine.Nfa)
    def body(s: String) = s.linesIterator.filterNot(_.startsWith("Engine:")).mkString("\n")
    assert(t.contains("Engine: DFA ("), t)
    assertEquals(body(nfaLines.result().mkString("\n")), body(t))
  }

  test("HW 2, problem 2: the spec lexes abaabbaba as simulated by hand") {
    val hw2 = LexSpec.load[hw.K]("hw2.slex")
    assertEquals(kinds(hw2.tokenize("abaabbaba")),
      List("Tok1:aba", "Tok2:abba", "Tok3:b", "Tok3:a"))
    // the first munch runs six characters deep and gives three back
    val lines = List.newBuilder[String]
    hw2.tokenize("abaabbaba", trace = lines.addOne)
    assert(lines.result().exists(_.contains("takes 3 chars  (backing up 3)")))
  }

  // --- Inspecting the Machine ----------------------------------------------

  test("the rel.slex dump: the rule table and the tagged combined NFA") {
    val rel = LexSpec.load[laws.RelK]("rel.slex")
    val d = rel.dump()
    assert(d.contains("Rules (listing order breaks ties):"), d)
    assert(d.contains("NFA (11 states"), d)
    assert(d.contains("accepts #1 \"<\""), d)
    assert(d.contains("accepts #2 \"<=\""), d)
    // the DFA the subset construction builds from it: the 3-state machine
    assert(d.contains("DFA (3 states"), d)
    assert(d.contains("0 = {0, 1, 2, 3, 5, 6, 7}"), d)
    assert(d.contains("2 = {10}  accepts #2 \"<=\""), d)
    // the handout's state counts for the bigger example
    assert(miniSpec.dump().contains("NFA (103 states"))
    assert(miniSpec.dump().contains("DFA (30 states"))
  }

  // --- A Complete Example: A Miniature IC Lexer ---------------------------

  val miniSpec = LexSpec.load[mini.K]("mini.slex")

  test("the miniature IC lexer") {
    assertEquals(
      kinds(miniSpec.tokenize("while (i <= 10) { i = i1; } // done")),
      List("WHILE:while", "LP:(", "ID:i", "LEQ:<=", "NUM:10", "RP:)",
           "LB:{", "ID:i", "ASSIGN:=", "ID:i1", "SEMI:;", "RB:}"))
  }

  test("skip rules discard, but positions stay honest") {
    val ts = miniSpec.tokenize("12\n  34")
    assertEquals(ts(0).pos, Pos(1, 1))
    assertEquals(ts(1).pos, Pos(2, 3))
  }

  test("unmatched input is an error, not a crash") {
    val e = intercept[LexError](miniSpec.tokenize("12$"))
    assertEquals(e.pos, Pos(1, 3))
    assert(e.message.contains("$"))
  }

  test("scanning is one token at a time") {
    val scan = miniSpec.scan("if x")
    assertEquals(scan.next().map(_.text), Some("if"))
    assertEquals(scan.next().map(_.text), Some("x"))
    assertEquals(scan.next(), None)
  }

  // --- Actions: The Escape Hatch ------------------------------------------

  val strSpec = LexSpec.load[strings.K]("strings.slex")

  test("transforming the lexeme: the quotes come off in the block") {
    assertEquals(strSpec.tokenize("\"hi\"").head.text, "hi")
  }

  test("checking and erroring: values ride in the token") {
    assertEquals(strSpec.tokenize("42").head.value, 42)
    val e = intercept[LexError](strSpec.tokenize("99999999999999999999"))
    assert(e.message.contains("out of range"))
  }

  test("the catch-all idiom for unterminated strings") {
    val e = intercept[LexError](strSpec.tokenize("\"oops"))
    assertEquals(e.pos, Pos(1, 1))
    assert(e.message.contains("unterminated"))
  }
