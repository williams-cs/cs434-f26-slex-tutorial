package scup

/**
 * What a grammar spec needs to know about its token language: the
 * kind enum's members by name, the `%token` literal table (so
 * productions write `"while"` for a terminal), display names for
 * parse errors, and the enum's class name (typing the generated
 * actions).  Produced by `slex.LexSpec#binding`; host-less tools can
 * synthesize one (see `GrammarSpec.loadStandalone`).
 */
final case class TokenBinding[K](
    kinds: Map[String, K],
    literals: Map[String, K],
    kindNames: Map[K, String],
    kindsFqcn: String)
