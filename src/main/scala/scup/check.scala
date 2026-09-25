package scup

/**
 * The scup check CLI: load each grammar spec standalone (terminals
 * become synthetic kinds, so no lexer spec or enum is needed) and run
 * the LL(1) analysis, reporting spec problems in `path:line: message`
 * form and conflicts with the DSL's exact diagnostics.  `-dump` also
 * prints the FIRST/FOLLOW sets and the LL(1) table -- the HW grammar
 * self-check flow.
 *
 *   sbt "runMain scup.check hw2.scup"
 *   sbt "runMain scup.check -dump hw2.scup"
 */
object check:
  def main(args: Array[String]): Unit =
    val (flags, paths) = args.partition(_.startsWith("-"))
    val dump = flags.contains("-dump")
    if paths.isEmpty then
      System.err.println("usage: scup.check [-dump] <spec.scup> ...")
      sys.exit(2)
    var failed = false
    for path <- paths do
      var model: Option[SpecAssemble.ScupSpecModel] = None
      try
        val g = GrammarSpec.loadStandalone(path)
        model = Some(g.model)
        g.check()
        println(s"$path: OK -- LL(1), ${g.model.rules.length} rules " +
          s"(start: ${g.startName})")
        // a misspelled nonterminal silently becomes a terminal in
        // standalone mode; flag anything that looks like a rule name
        for t <- GrammarSpec.impliedTerminals(g.model) if t.exists(_.isLower) do
          println(s"$path: note: no rule defines '$t', so it is treated as " +
            "a terminal -- misspelled nonterminal?")
        if dump then println(g.dump())
      catch
        case e: SpecAssemble.SpecError =>
          System.err.println(e.getMessage)
          failed = true
        case e: GrammarError =>
          // anchor the conflict at the offending rule's line: the
          // message names its rules as 'name'
          val line = model.map { m =>
            val named = "'([A-Za-z_][A-Za-z0-9_]*)'".r
              .findAllMatchIn(e.getMessage).map(_.group(1)).toList
            named.flatMap(n => m.rules.find(_.name == n)).headOption
              .map(_.line).getOrElse(1)
          }.getOrElse(1)
          System.err.println(s"$path:$line: ${e.getMessage}")
          failed = true
        case e: Exception =>
          System.err.println(s"$path: ${e.getMessage}")
          failed = true
    if failed then sys.exit(1)
