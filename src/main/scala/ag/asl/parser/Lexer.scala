package ag.asl.parser

import scala.annotation.tailrec
import upickle.default.ReadWriter

case class Position(indent: Int, row: Int, column: Int) derives ReadWriter

///////////
// Token //
///////////

sealed trait Token derives ReadWriter {
  def position: Position
}

case class Bits(str: String, position: Position) extends Token
    derives ReadWriter

case class Decimal(str: String, position: Position) extends Token
    derives ReadWriter
case class Hex(str: String, position: Position) extends Token derives ReadWriter
case class Real(str: String, position: Position) extends Token
    derives ReadWriter

// Identifiers that act as ops
val id_ops = Set(
  "AND",
  "DIV",
  "EOR",
  "IN",
  "MOD",
  "NOT",
  "OR"
)

val double_ops = Set(
  "+:",
  "<<",
  ">>",
  "!=",
  "==",
  "&&",
  "||",
  "<=",
  ">=",
  ".."
).map(s => (s(0), s(1)))

val single_ops = "&+-*/^.()[]{},=;|&!<>:".toSet

case class Operator(str: String, position: Position) extends Token
    derives ReadWriter

case class Identifier(str: String, position: Position) extends Token
    derives ReadWriter
val identifier_start = 'a'.to('z').toSet ++ 'A'.to('Z').toSet ++ Set('_')
val identifier_char = identifier_start ++ '0'.to('9').toSet

case class Str(str: String, position: Position) extends Token derives ReadWriter

val bits_chars = Set('0', '1', 'x', ' ')
var hex_chars = "0123456789abcdefABCDEF".toSet

class Lexer private (private var chars: LazyList[Char])
    extends Iterator[Token] {
  private var line = 0
  private var col = 0
  private var indent = -1

  // Skip comments and spaces
  @tailrec
  private final def skip(): Unit = chars match {
    case '\n' #:: rest =>
      line += 1
      col = 0
      indent = -1
      chars = rest
      skip()
    case ' ' #:: rest =>
      col += 1
      chars = rest
      skip()
    case '/' #:: '*' #:: rest =>
      @tailrec
      def skip_comment(): Unit = chars match {
        case '*' #:: '/' #:: rest =>
          col += 2
          chars = rest
        case '\n' #:: rest =>
          line += 1
          col = 0
          indent = -1
          chars = rest
          skip_comment()
        case ' ' #:: rest =>
          col += 1
          if (indent == -1) indent = col
          chars = rest
          skip_comment()
        case _ #:: rest =>
          col += 1
          chars = rest
          skip_comment()
      }
      col += 2
      chars = rest
      skip_comment()
      skip()
    case '/' #:: '/' #:: rest =>
      col += 2
      chars = rest

      @tailrec
      def skip_comment(): Unit = chars match {
        case '\n' #:: _ =>
        case _ #:: rest =>
          col += 1
          chars = rest
          skip_comment()
        case LazyList() =>
      }
      skip_comment()
      skip()
    case _ =>
      if (indent == -1) {
        indent = col
      }
    // done
  }

  override def hasNext: Boolean = {
    skip()
    chars.nonEmpty
  }

  private def error(str: String): Nothing = {
    throw IllegalArgumentException(s"""
         |$str
         |at $line:$col
         |${chars.mkString}
         |""".stripMargin)
  }

  // def here: Position = Position(indent, line, col)

  override def next: Token = {
    skip()
    val here = Position(indent, line, col)
    val t = chars match {
      case '\'' #:: rest =>
        val bits = rest.takeWhile(ch => bits_chars.contains(ch))
        val after_the_bits = rest.drop(bits.length)
        if (after_the_bits.headOption.contains('\'')) {
          col += bits.length + 2
          chars = after_the_bits.drop(1)
          Bits(bits.mkString, here)
        } else {
          col += bits.length + 1
          chars = after_the_bits
          error("expected closing '")
        }

      case '"' #:: rest =>
        val str = rest.takeWhile(ch => (ch != '"') && (ch != '\n'))
        val after_the_str = rest.drop(str.length)
        if (after_the_str.headOption.contains('"')) {
          col += str.length + 2
          chars = after_the_str.drop(1)
          Str(str.mkString, here)
        } else {
          col += str.length + 1
          error("expected closing \"")
        }

      case '0' #:: 'x' #:: rest =>
        val str = rest.takeWhile(ch => hex_chars.contains(ch)).mkString
        chars = rest.drop(str.length)
        col += 2 + str.length
        Hex(str, here)

      case ch1 #:: ch2 #:: rest if double_ops.contains((ch1, ch2)) =>
        col += 2
        chars = rest
        Operator(s"${ch1}$ch2", here)

      case ch #:: rest if single_ops.contains(ch) =>
        col += 1
        chars = rest
        Operator(ch.toString, here)

      case ch #:: rest if identifier_start.contains(ch) =>
        val id_chars = rest.takeWhile(identifier_char.contains)
        val id = (ch #:: id_chars).mkString
        col += id.length
        chars = rest.drop(id_chars.size)
        if (id_ops.contains(id)) {
          Operator(id, here)
        } else {
          Identifier(id, here)
        }

      case ch #:: rest if ch.isDigit =>
        val decimal_chars = rest.takeWhile(_.isDigit)
        val after_digits = rest.drop(decimal_chars.size)
        after_digits match {
          case '.' #:: start_of_fraction
              if !start_of_fraction.headOption.contains('.') =>
            // real number
            val fraction_chars = start_of_fraction.takeWhile(_.isDigit)
            col += 1 + decimal_chars.size + 1 + fraction_chars.size
            chars = start_of_fraction.drop(fraction_chars.size)
            Real(
              s"$ch${decimal_chars.mkString}.${fraction_chars.mkString}",
              here
            )
          case _ =>
            val decimal = (ch #:: decimal_chars).mkString
            col += decimal.length
            chars = rest.drop(decimal_chars.size)
            Decimal(decimal, here)
        }
      case x #:: rest =>
        error(s"unrecognized character '$x' ${x.toInt}")
    }
    // pprint.pprintln(t)
    t
  }
}

object Lexer {
  def from(chars: IterableOnce[Char]): LazyList[Token] = from(
    LazyList.from(chars)
  )
  def from(chars: LazyList[Char]): LazyList[Token] = LazyList.from(Lexer(chars))
}
