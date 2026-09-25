package scup

/**
 * The generic parse-tree node built for action-free grammar rules: a
 * `.scup` file with no blocks and no %types still loads, LL(1)-checks,
 * and parses -- to a tree of these, labeled with rule names.  (This is
 * the HW grammar self-check mode: type in a grammar as plain text and
 * check it with zero Scala.)
 */
final case class STree(rule: String, children: List[Any], pos: Pos):

  /** An indented one-node-per-line rendering. */
  def show: String =
    val sb = StringBuilder()
    def go(v: Any, depth: Int): Unit =
      val pad = "  " * depth
      v match
        case t: STree =>
          sb.append(pad).append(t.rule).append('\n')
          t.children.foreach(go(_, depth + 1))
        case null => sb.append(pad).append("-").append('\n')
        case xs: List[?] => xs.foreach(go(_, depth))
        case other => sb.append(pad).append(other.toString).append('\n')
    go(this, 0)
    sb.toString

object STree:
  /** The position of the first position-bearing value in `vs`. */
  def firstPos(vs: Seq[Any]): Pos =
    def posOf(v: Any): Option[Pos] = v match
      case t: Tokenish => Some(t.pos)
      case s: STree if s.pos != Pos(0, 0) => Some(s.pos)
      case xs: List[?] => xs.iterator.flatMap(posOf).nextOption()
      case _ => None
    vs.iterator.flatMap(posOf).nextOption().getOrElse(Pos(0, 0))
