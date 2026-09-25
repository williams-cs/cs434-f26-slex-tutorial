package scup



/**
 * The generic token produced by a spec-driven lexer: its kind (a
 * member of the host's TokenKind enum), the matched text, where it
 * began, and an optional computed value (a parsed integer, an
 * unescaped string, a boolean) placed there by the rule's action.
 *
 * Extending scup.Tokenish (which just requires pos) is what lets scup
 * grammars consume a sequence of Tokens.
 */
final case class Token[K](kind: K, text: String, pos: Pos, value: Any = ())
    extends Tokenish:

  /** The line on which this token appeared. */
  def line: Int = pos.line

  /** The column at which this token began. */
  def col: Int = pos.col

  /** The stored value of an INTLIT-style token (the lexer action must
   *  have put an Int in `value`). */
  def intValue: Int = value.asInstanceOf[Int]

  /** The stored value of a BOOLLIT-style token. */
  def boolValue: Boolean = value.asInstanceOf[Boolean]

  override def toString: String = s"[$kind,$text,${pos.line}]"
