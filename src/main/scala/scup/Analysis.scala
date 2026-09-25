package scup

/**
 * The LL(1) analysis: nullable / FIRST / FOLLOW and the prediction
 * table, computed by the standard fixpoints (Dragon 4.4) over the
 * grammar's productions, with EBNF modifiers already desugared to
 * synthesized rules.  Conflicts and left recursion are rejected here,
 * when a grammar is first used.
 */
private[scup] trait GrammarAnalysis[T <: Tokenish, K]:
  this: Grammar[T, K] =>

  /** EOF is the terminal `None`; real terminals are `Some(kind)`. */
  private[scup] type La = Option[K]
  private[scup] def showLa(la: La): String =
    la.fold("end of input")(showKind)

  private val analysesCache = collection.mutable.Map[Rule[?], Analysis]()
  private[scup] def analysis(start: Rule[?]): Analysis =
    baptize()   // resolve val-derived rule names before any reporting
    analysesCache.getOrElseUpdate(start, new Analysis(start))

  private[scup] final class Analysis(val start: Rule[?]):

    /** Every rule reachable from the start symbol, in discovery order. */
    val rules: IndexedSeq[Rule[?]] =
      val seen  = collection.mutable.LinkedHashSet[Rule[?]]()
      def visit(r: Rule[?]): Unit =
        if seen.add(r) then
          for p <- r.prods; s <- p.syms do s match
            case sub: Rule[?] => visit(sub)
            case _            => ()
      visit(start)
      seen.toIndexedSeq

    /*--------------------------- nullable ----------------------------*/

    val nullable: Set[Rule[?]] =
      val known = collection.mutable.Set[Rule[?]]()
      var changed = true
      while changed do
        changed = false
        for r <- rules if !known(r) do
          if r.prods.exists(_.syms.forall {
                case sub: Rule[?] => known(sub)
                case _            => false
              })
          then { known += r; changed = true }
      known.toSet

    private def seqNullable(syms: List[Sym[?]]): Boolean =
      syms.forall { case r: Rule[?] => nullable(r); case _ => false }

    /*----------------------- repetition sanity -----------------------*/

    // x.rep of a symbol that can match the empty string would loop
    // forever (each iteration consumes nothing).  Checked before left
    // recursion so the error names the real mistake.
    locally {
      for r <- rules if r.loop != LoopKind.None do
        val recursive = r.prods(0).syms
        if seqNullable(recursive.dropRight(1)) then
          throw GrammarError(
            s"in '${r.name}': the repeated symbols can match the empty " +
            "string, so the repetition would never terminate")
    }

    /*------------------------- left recursion ------------------------*/

    // R -> S if S can appear leftmost in a derivation from R (i.e. S
    // is preceded in some production only by nullable symbols).  A
    // rule that can reach itself is left-recursive.
    locally {
      val leftEdges: Map[Rule[?], List[Rule[?]]] =
        rules.map { r =>
          r -> (for
            p <- r.prods.toList
            i <- p.syms.indices
            if seqNullable(p.syms.take(i))
            s = p.syms(i)
            sub <- s match { case x: Rule[?] => Some(x); case _ => None }
          yield sub)
        }.toMap
      def cycleFrom(r: Rule[?]): Option[List[Rule[?]]] =
        def dfs(cur: Rule[?], path: List[Rule[?]], seen: Set[Rule[?]]): Option[List[Rule[?]]] =
          leftEdges(cur).collectFirst(Function.unlift { next =>
            if next eq r then Some((r :: path).reverse :+ r)
            else if seen(next) then None
            else dfs(next, next :: path, seen + next)
          })
        dfs(r, List(r), Set(r))
      for r <- rules; cycle <- cycleFrom(r) do
        throw GrammarError(
          s"left recursion: ${cycle.map(_.name).mkString(" -> ")}; " +
          "an LL(1) grammar cannot decide with one token of lookahead -- " +
          "rewrite with a %left block and a %binary rule (an operator chain) or right recursion")
    }

    /*---------------------------- FIRST ------------------------------*/

    val first: Map[Rule[?], Set[K]] =
      val f = collection.mutable.Map[Rule[?], Set[K]]().withDefaultValue(Set.empty)
      var changed = true
      while changed do
        changed = false
        for r <- rules; p <- r.prods do
          var add = Set.empty[K]
          var i = 0
          var go = true
          while go && i < p.syms.length do
            p.syms(i) match
              case Term(k)      => add += k; go = false
              case sub: Rule[?] => add ++= f(sub); go = nullable(sub)
            i += 1
          if !add.subsetOf(f(r)) then { f(r) ++= add; changed = true }
      f.toMap.withDefaultValue(Set.empty)

    private def seqFirst(syms: List[Sym[?]]): Set[K] =
      var acc = Set.empty[K]
      var i = 0
      var go = true
      while go && i < syms.length do
        syms(i) match
          case Term(k)      => acc += k; go = false
          case sub: Rule[?] => acc ++= first(sub); go = nullable(sub)
        i += 1
      acc

    /*---------------------------- FOLLOW -----------------------------*/

    val follow: Map[Rule[?], Set[La]] =
      val f = collection.mutable.Map[Rule[?], Set[La]]().withDefaultValue(Set.empty)
      f(start) = Set(None)          // $ follows the start symbol
      var changed = true
      while changed do
        changed = false
        for r <- rules; p <- r.prods do
          for i <- p.syms.indices do
            p.syms(i) match
              case sub: Rule[?] =>
                val rest = p.syms.drop(i + 1)
                var add: Set[La] = seqFirst(rest).map(Some(_))
                if seqNullable(rest) then add ++= f(r)
                if !add.subsetOf(f(sub)) then { f(sub) ++= add; changed = true }
              case _ => ()
      f.toMap.withDefaultValue(Set.empty)

    /*------------------------- the LL(1) table -----------------------*/

    /** For each rule, lookahead terminal -> production index. */
    val table: Map[Rule[?], Map[La, Int]] =
      rules.map { r =>
        val entries = collection.mutable.Map[La, Int]()
        // (production index, its predict set, whether via FOLLOW)
        val predicts: Seq[(Int, Set[La], Boolean)] =
          r.prods.indices.map { i =>
            val p    = r.prods(i)
            val own  = seqFirst(p.syms).map(Some(_): La)
            val viaF = seqNullable(p.syms)
            (i, own, viaF)
          }
        // Two productions that can both match nothing can never be
        // distinguished, greedy or not.
        val nullableProds = predicts.collect { case (i, _, true) => i }
        if nullableProds.length > 1 then
          throw GrammarError(
            s"rule '${r.name}' is not LL(1): productions " +
            s"${nullableProds.map(_ + 1).mkString(" and ")} can both match nothing")
        for (i, own, _) <- predicts; la <- own do
          entries.get(la) match
            case Some(j) =>
              throw GrammarError(
                s"rule '${r.name}' is not LL(1): productions ${j + 1} and ${i + 1} " +
                s"can both begin with ${showLa(la)} (FIRST/FIRST conflict); " +
                "left-factor the common prefix")
            case None => entries(la) = i
        for (i, _, viaF) <- predicts if viaF; la <- follow(r) do
          entries.get(la) match
            case Some(j) if j != i =>
              if r.greedy then ()   // the consuming production wins
              else
                throw GrammarError(
                  s"rule '${r.name}' is not LL(1): production ${j + 1} can begin " +
                  s"with ${showLa(la)}, which can also follow '${r.name}' when " +
                  s"production ${i + 1} matches nothing (FIRST/FOLLOW conflict); " +
                  "left-factor, or use greedyRule if consuming here is intended " +
                  "(as for the dangling else)")
            case _ => entries(la) = i
        r -> entries.toMap
      }.toMap

    /** The terminals that could come next in rule r, for error messages. */
    def expected(r: Rule[?]): List[String] =
      table(r).keys.toList.map(showLa).sorted

    /*----------------------------- dump ------------------------------*/

    private def showSyms(syms: List[Sym[?]]): String =
      if syms.isEmpty then "%empty" else syms.map(_.toString).mkString(" ")

    /** Global production numbers, in listing order -- the `(n)` used
     *  by the dump's table and by parse traces. */
    lazy val prodNum: Map[(Rule[?], Int), Int] =
      val b = Map.newBuilder[(Rule[?], Int), Int]
      var n = 1
      for r <- rules; i <- r.prods.indices do
        b += (r, i) -> n
        n += 1
      b.result()

    /** The numbered production listing, as printed by the dump (and
     *  at the top of a parse trace). */
    def productionListing: String =
      val sb = StringBuilder()
      sb ++= "Productions:\n"
      for r <- rules; i <- r.prods.indices do
        val lhs = if i == 0 then f"${r.name}%12s ::=" else f"${" "}%12s   |"
        sb ++= f"  (${prodNum((r, i))}%2d) $lhs ${showSyms(r.prods(i).syms)}\n"
      sb.result()

    /** The whole analysis, printable: productions, sets, and table. */
    def dump: String =
      val sb = StringBuilder()
      sb ++= s"Grammar (start symbol: ${start.name})\n\n"
      sb ++= productionListing
      sb ++= "\nNullable: " +
        (rules.filter(nullable).map(_.name) match
          case Seq() => "(none)"
          case ns    => ns.mkString(", ")) + "\n"
      sb ++= "\nFIRST:\n"
      for r <- rules do
        sb ++= f"  ${r.name}%12s : ${first(r).toList.map(showKind).sorted.mkString(", ")}\n"
      sb ++= "\nFOLLOW:\n"
      // $ (end of input) prints last, as in the table's columns.
      for r <- rules do
        val las = follow(r).toList.sortBy(la => (la.isEmpty, showLa(la)))
        sb ++= f"  ${r.name}%12s : ${las.map(_.fold("$")(showKind)).mkString(", ")}\n"
      sb ++= "\nLL(1) table (entries are production numbers; $ is end of input):\n\n"
      // One column per lookahead that predicts anything, terminals in
      // display order with $ last -- the Dragon book's Figure 4.17.
      val cols = rules.flatMap(r => table(r).keys).distinct
        .sortBy(la => (la.isEmpty, showLa(la)))
      def head(la: La) = la.fold("$")(showKind)
      val widths = cols.map(la => head(la).length max 2)
      def cell(s: String, w: Int) = " " * (2 + w - s.length) + s
      sb ++= " " * 14
      for (la, w) <- cols.zip(widths) do sb ++= cell(head(la), w)
      sb ++= "\n"
      for r <- rules do
        sb ++= f"  ${r.name}%12s"
        for (la, w) <- cols.zip(widths) do
          sb ++= cell(table(r).get(la).fold(".")(i => prodNum((r, i)).toString), w)
        sb ++= "\n"
      sb.result()
