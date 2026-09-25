package slex

/**
 * The slex check CLI: load each spec file and report problems, in a
 * `path:line: message` format editors can parse.  `-dump` also
 * prints the rule list, the combined NFA the spec compiles to --
 * every state, ε-edge, symbol edge, and tagged accept state -- and
 * the DFA built from it by subset construction.
 *
 *   sbt "runMain slex.check ic.slex"
 *   sbt "runMain slex.check -dump ic.slex"
 */
object check:
  def main(args: Array[String]): Unit =
    val (flags, paths) = args.partition(_.startsWith("-"))
    val dump = flags.contains("-dump")
    if paths.isEmpty then
      System.err.println("usage: slex.check [-dump] <spec.slex> ...")
      sys.exit(2)
    var failed = false
    for path <- paths do
      try
        val spec = LexSpec.loadReflect(path)
        val nRules = spec.model.rules.length
        println(s"$path: OK ($nRules rules, ${spec.model.actionBlocks.length} actions)")
        println(s"$path: NFA ${spec.lexer.nfa.size} states, DFA ${spec.lexer.dfa.size} states")
        // which enum members have no %token row: their tokens must
        // come from action blocks
        val fixedKinds = spec.model.rules.collect {
          case scup.SpecAssemble.FixedRule(k, _, _) => k
        }.toSet
        val fromBlocks = spec.kinds.keys.filterNot(fixedKinds).toList
        if fromBlocks.nonEmpty then
          println(s"$path: kinds produced by action blocks " +
            s"(no %token row): ${fromBlocks.mkString(", ")}")
        if dump then
          println()
          println(spec.dump())
      catch
        case e: scup.SpecAssemble.SpecError =>
          System.err.println(e.getMessage)
          failed = true
        case e: Exception =>
          System.err.println(s"$path: ${e.getMessage}")
          failed = true
    if failed then sys.exit(1)
