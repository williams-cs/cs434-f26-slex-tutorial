package scup

/**
 * A source position (1-based line and column).
 */
final case class Pos(line: Int, col: Int):
  /** True if this position is at or beyond `other` in the source. */
  def >=(other: Pos): Boolean =
    line > other.line || (line == other.line && col >= other.col)

  override def toString = s"line $line, column $col"

object Pos:
  val start = Pos(1, 1)

/**
 * Anything a Parser can consume must carry the position where it
 * appeared in the source, so parsers can report errors and record
 * positions in AST nodes.
 */
trait Tokenish:
  def pos: Pos

/**
 * An immutable cursor into a sequence of tokens.  Parsers never mutate
 * the input; they return a new Input advanced past what they consumed.
 * That is what makes backtracking in `|` trivial: just re-use the old
 * Input.
 */
final case class Input[T <: Tokenish](toks: IndexedSeq[T], idx: Int = 0):
  /** Have we consumed every token? */
  def atEnd: Boolean = idx >= toks.length

  /** The next token, if any. */
  def current: Option[T] = if atEnd then None else Some(toks(idx))

  /** The input with the next token consumed. */
  def advance: Input[T] = Input(toks, idx + 1)

  /**
   * The position of the next token.  At the end of input we report the
   * position of the last token (good enough for "unexpected end of
   * input" messages).
   */
  def pos: Pos =
    if !atEnd then toks(idx).pos
    else if toks.nonEmpty then toks.last.pos
    else Pos.start
