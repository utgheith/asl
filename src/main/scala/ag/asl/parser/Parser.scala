package ag.asl.parser

import scala.annotation.tailrec
import upickle.default.ReadWriter

sealed trait TopLevel derives ReadWriter

sealed trait Type derives ReadWriter

sealed trait Expression derives ReadWriter

sealed trait CanCast extends Expression derives ReadWriter

sealed trait Statement derives ReadWriter {
  def isSimple: Boolean
}

case class Angular(e: Seq[Expression]) extends Expression derives ReadWriter

case class ArrayRef(path: Expression, e: Seq[Expression]) extends Expression
    derives ReadWriter

case class AslSet(e: Seq[Expression]) extends Expression derives ReadWriter

case class Cast(`type`: Type, value: CanCast) extends Expression
    derives ReadWriter

case class DecimalExpression(str: String) extends Expression
    derives ReadWriter {
  override def toString: String = str
}

case class HexExpression(str: String) extends Expression derives ReadWriter {
  override def toString: String = s"0x$str"
}

case class RealExpression(str: String) extends Expression derives ReadWriter {
  override def toString: String = str
}

case class BinaryExpression(lhs: Expression, op: String, rhs: Expression)
    extends Expression derives ReadWriter

case class BitsExpression(bits: String) extends Expression derives ReadWriter

case class Call(exp: Expression, args: Seq[Expression]) extends Expression
    derives ReadWriter

case class IfExpression(conds: Seq[(Expression, Expression)], f: Expression)
    extends Expression derives ReadWriter

case class ImplementationDefined(str: Option[String])
    extends Statement
    with CanCast derives ReadWriter {
  override val isSimple: Boolean = true
}

case class TupleValue(exps: Seq[Expression]) extends Expression
    derives ReadWriter

case class UnaryExpression(op: String, e: Expression) extends Expression
    derives ReadWriter

case object Unknown extends CanCast //derives ReadWriter

case class VariableName(id: Identifier) extends Expression derives ReadWriter

case class ArrayType(dims: Seq[Expression], element_item: Type) extends Type
    derives ReadWriter

case class BitsType(width: Expression) extends Type derives ReadWriter

case class NamedType(id: Identifier) extends Type derives ReadWriter

case class TupleType(ts: Seq[Type]) extends Type derives ReadWriter

case class FormalArg(`type`: Type, mod: Option[Operator], id: Identifier)
    derives ReadWriter

case class ScopedName(ids: Seq[Identifier], bits: Seq[Identifier])
    extends Expression derives ReadWriter

case class Select(exp: Expression, id: Identifier) extends Expression
    derives ReadWriter

case class Slice(exp: Expression, es: Seq[Expression]) extends Expression
    derives ReadWriter

case class FunctionDecl(
    `type`: Option[Type],
    name: ScopedName,
    args: Seq[FormalArg]
) extends TopLevel derives ReadWriter

case class FunctionDef(
    `type`: Option[Type],
    name: ScopedName,
    args: Seq[FormalArg],
    statement: Statement
) extends TopLevel derives ReadWriter

case class ArrayDeclaration(`type`: Type, name: ScopedName, dim: Expression)
    extends TopLevel derives ReadWriter

case class ArrayGetter(
    `type`: Type,
    name: ScopedName,
    args: Seq[FormalArg],
    statement: Statement
) extends TopLevel derives ReadWriter

case class ArraySetter(
    name: ScopedName,
    args: Seq[FormalArg],
    rhs: FormalArg,
    body: Statement
) extends TopLevel derives ReadWriter

case class RegGetter(`type`: Type, name: ScopedName, body: Statement)
    extends TopLevel derives ReadWriter

case class RegSetter(name: ScopedName, rhs: FormalArg, body: Statement)
    extends TopLevel derives ReadWriter

case class MemberDeclaration(`type`: Type, identifier: Identifier)
    derives ReadWriter

case class StructDeclaration(id: Identifier, members: Seq[MemberDeclaration])
    extends TopLevel derives ReadWriter

case class TypeDeclaration(id: Identifier, `type`: Option[Type])
    extends TopLevel derives ReadWriter

////////////////////// assignment ///////////////////////

case class AssertStatement(e: Expression) extends Statement derives ReadWriter {
  val isSimple: Boolean = true
}

case class Assignment(lhs: Expression, rhs: Expression) extends Statement
    derives ReadWriter {
  val isSimple: Boolean = true
}
case class Block(statements: Seq[Statement]) extends Statement
    derives ReadWriter {
  val isSimple: Boolean = false
}
case class WhenStatement(exp: Seq[Expression], code: Statement)
    extends Statement derives ReadWriter {
  val isSimple: Boolean = false
}
case class CaseStatement(
    exp: Expression,
    choices: Seq[WhenStatement],
    otherwise: Option[Statement]
) extends Statement derives ReadWriter {
  val isSimple: Boolean = false
}
case class Declaration(
    modifiers: Seq[Identifier],
    `type`: Type,
    id: Seq[Identifier],
    init: Option[Expression]
) extends Statement
    with TopLevel derives ReadWriter {
  val isSimple: Boolean = true
}

case class ConstDeclaration(ids: Seq[Identifier], expression: Expression)
    extends Statement derives ReadWriter {
  val isSimple: Boolean = true
}

case class ForStatement(
    down: Boolean,
    v: Identifier,
    start: Expression,
    end: Expression,
    body: Statement
) extends Statement derives ReadWriter {
  val isSimple: Boolean = false
}

case class IfStatement(
    if_parts: Seq[(Expression, Statement)],
    else_part: Option[Statement]
) extends Statement derives ReadWriter {
  val isSimple: Boolean = false
}

case class CallStatement(name: ScopedName, args: Seq[Expression])
    extends Statement derives ReadWriter {
  val isSimple: Boolean = true
}

case class RepeatStatement(body: Statement, until: Expression) extends Statement
    derives ReadWriter {
  val isSimple: Boolean = true
}

case class Return(e: Option[Expression]) extends Statement derives ReadWriter {
  val isSimple: Boolean = true
}

case class SeeStatement(str: String) extends Statement derives ReadWriter {
  val isSimple: Boolean = true
}

case object Undefined extends Statement { // derives ReadWriter {
  val isSimple: Boolean = true
}

case object Unpredictable extends Statement { // derives ReadWriter {
  val isSimple: Boolean = true
}

case class WhileStatement(cond: Expression, body: Statement) extends Statement
    derives ReadWriter {
  val isSimple: Boolean = false
}

case class EnumDeclaration(id: Identifier, choices: Seq[Identifier])
    extends TopLevel derives ReadWriter

case class ParseError(msg: String, lexer: LazyList[Token])
    extends Exception(msg) {}

val p0 = Set[String]("!", "-", "NOT")
val p1 = Set[String]("^")
val p2 = Set[String]("*", "/", "DIV", "MOD")
val p3 = Set[String]("+", "-")
val p4 = Set[String]("<", ">", "==", "!=", ">=", "<=", ":", "+:")
val p5 = Set[String]("||", "&&", "IN", "AND", "EOR", "OR", "..", "<<", ">>")

val magic_values: Set[String] = Set("IMPLEMENTATION_DEFINED")

val keywords: Set[String] = Set(
  "array",
  "assert",
  "bit",
  "bits",
  "case",
  "constant",
  "do",
  "downto",
  "enumeration",
  "for",
  "if",
  "is",
  "elsif",
  "else",
  "of",
  "otherwise",
  "repeat",
  "return",
  "SEE",
  "then",
  "to",
  "type",
  "UNDEFINED",
  "UNKNOWN",
  "UNPREDICTABLE",
  "until",
  "when",
  "while"
) ++ magic_values

class Parser(var lexer: LazyList[Token]) {

  private def error(msg: String): Nothing = throw ParseError(msg, lexer)

  private def expect[A](o: Option[A], what: String): A = o match {
    case Some(a) => a
    case None    => error(s"expecting '$what'")
  }

  private def expect(str: String): Token = lexer match {
    case (out @ Operator(s, _)) #:: rest if s == str =>
      lexer = rest
      out
    case (out @ Identifier(s, _)) #:: rest if s == str =>
      lexer = rest
      out
    case _ =>
      error(s"expecting '$str")
  }

  private def rollback[A](f: => Option[A]): Option[A] = {
    val old = lexer
    val out = f
    if (out.isEmpty) {
      lexer = old
    }
    out
  }

  def repeat[A](
      f: => Option[A],
      sep: (Token => Boolean) | Null = null
  ): Seq[A] = {
    @tailrec
    def loop(acc: Seq[A]): Seq[A] = f match {
      case Some(a) =>
        lexer match {
          case t #:: rest if sep == null || sep(t) =>
            if (sep != null) lexer = rest
            loop(a +: acc)
          case _ =>
            a +: acc
        }
      case None =>
        acc
    }
    loop(Seq()).reverse
  }

  def repeat[A](f: => Option[A], sep: String): Seq[A] = repeat(
    f,
    {
      case Operator(str, _)   => str == sep
      case Identifier(str, _) => str == sep
      case _                  => false
    }
  )

  private def identifier(str: Option[String] = None): Option[Identifier] =
    lexer match {
      case (id: Identifier) #:: rest
          if (!keywords
            .contains(id.str)) && (str.isEmpty || str.contains(id.str)) =>
        lexer = rest
        Some(id)
      case _ =>
        None
    }

  private def keyword(str: String): Option[Identifier] = lexer match {
    case (id: Identifier) #:: rest
        if keywords.contains(id.str) && str == id.str =>
      lexer = rest
      Some(id)
    case _ =>
      None
  }

  private def operator(op: String | Null = null): Option[Operator] =
    lexer match {
      case (t: Operator) #:: rest if op == null | op == t.str =>
        lexer = rest
        Some(t)
      case _ =>
        None
    }

  private def operator(f: String => Boolean): Option[Operator] = lexer match {
    case (t: Operator) #:: rest if f(t.str) =>
      lexer = rest
      Some(t)
    case _ =>
      None
  }

  private def decimalExpression: Option[DecimalExpression] = {
    lexer match {
      case (d: Decimal) #:: rest =>
        lexer = rest
        Some(DecimalExpression(d.str))
      case _ =>
        None
    }
  }

  private def angular: Option[Expression] = rollback {
    for {
      _ <- operator("<")
      e = repeat(expression, ",")
      _ <- operator(">")
    } yield Angular(e)
  }

  private def paran: Option[Expression] = rollback {
    for {
      _ <- operator("(")
      e = repeat(expression, ",")
      _ <- operator(")")
    } yield if (e.size == 1) e.head else TupleValue(e)
  }

  private def unary: Option[Expression] = rollback {
    for {
      op <- operator(op => p0.contains(op))
      e <- expression
    } yield UnaryExpression(op.toString, e)
  }

  private def call(exp: Expression): Option[Expression] = rollback {
    for {
      tk <- operator("(")
      if tk.position.column != tk.position.indent // XXX ( can't be in a separate line
      args = expressions
      _ = expect(operator(")"), ")")
    } yield Call(exp, args)
  }

  private def array_ref(exp: Expression): Option[ArrayRef] = rollback {
    for {
      _ <- operator("[")
      args = expressions
      _ = expect(operator("]"), "]")
    } yield ArrayRef(exp, args)
  }

  private def elsif_expression: Option[(Expression, Expression)] = for {
    _ <- keyword("elsif")
    c <- expression
    _ <- keyword("then")
    e <- expression
  } yield (c, e)

  private def if_expression: Option[Expression] = for {
    _ <- keyword("if")
    c = expect(expression, "expression")
    _ = expect(keyword("then"), "then")
    t = expect(expression, "expression")
    elsif = repeat(elsif_expression)
    _ = expect(keyword("else"), "else")
    f = expect(expression, "expression")
  } yield IfExpression((c, t) +: elsif, f)

  private def bits_expression: Option[Expression] = lexer match {
    case (b: Bits) #:: rest =>
      lexer = rest
      Some(BitsExpression(b.str))
    case _ =>
      None
  }

  private def hex_expression: Option[HexExpression] = lexer match {
    case (h: Hex) #:: rest =>
      lexer = rest
      Some(HexExpression(h.str))
    case _ =>
      None
  }

  private def real_expression: Option[RealExpression] = lexer match {
    case (r: Real) #:: rest =>
      lexer = rest
      Some(RealExpression(r.str))
    case _ =>
      None
  }

  private def cast: Option[Cast] = rollback {
    for {
      t <- `type`
      kw <- unknown.orElse(implementation_defined)
    } yield Cast(t, kw)
  }

  private def implementation_defined: Option[ImplementationDefined] =
    lexer match {
      case Identifier("IMPLEMENTATION_DEFINED", _) #:: Str(str, _) #:: rest =>
        lexer = rest
        Some(ImplementationDefined(Some(str)))
      case Identifier("IMPLEMENTATION_DEFINED", _) #:: rest =>
        lexer = rest
        Some(ImplementationDefined(None))
      case _ =>
        None
    }

  private def unknown: Option[Unknown.type] = lexer match {
    case Identifier("UNKNOWN", _) #:: rest =>
      lexer = rest
      Some(Unknown)
    case _ =>
      None
  }

  private def slice(exp: Expression): Option[Slice] = rollback {
    for {
      _ <- operator("<")
      e = expressions
      _ <- operator(">")
    } yield Slice(exp, e)
  }

  private def asl_set: Option[AslSet] = for {
    _ <- operator("{")
    e = expressions
    _ = expect(operator("}"), "}")
  } yield AslSet(e)

  private def ignore: Option[Identifier] = lexer match {
    case Operator("-", pos) #:: Operator(op, _) #:: _
        if Set(",", ")").contains(op) =>
      lexer = lexer.drop(1)
      Some(Identifier("-", pos)) // XXX
    case _ =>
      None
  }

  private def primary_expression: Option[Expression] =
    paran
      .orElse(angular)
      .orElse(if_expression)
      .orElse(asl_set)
      .orElse(bits_expression)
      .orElse(hex_expression)
      .orElse(real_expression)
      .orElse(unary)
      .orElse(decimalExpression)
      .orElse(cast)
      .orElse(implementation_defined)
      .orElse(unknown)
      .orElse(scopedName)
      .orElse(ignore.map(VariableName.apply))

  private def select(exp: Expression): Option[Expression] = for {
    _ <- operator(".")
    id <- identifier()
  } yield Select(exp, id)

  @tailrec
  final private def modify(exp: Expression): Expression = lexer match {
    case Operator(".", _) #:: _ =>
      select(exp) match {
        case Some(exp) =>
          modify(exp)
        case None =>
          exp
      }
    case Operator("(", _) #:: _ =>
      call(exp) match {
        case Some(exp) =>
          modify(exp)
        case None =>
          exp
      }
    case Operator("[", _) #:: _ =>
      array_ref(exp) match {
        case Some(exp) =>
          modify(exp)
        case None =>
          exp
      }
    case Operator("<", _) #:: _ =>
      slice(exp) match {
        case Some(exp) =>
          modify(exp)
        case None =>
          exp
      }
    case _ =>
      exp
  }

  private def e0: Option[Expression] = primary_expression.map { exp =>
    modify(exp)
  }

  // e.g. exponentiation
  private def e1: Option[Expression] = e0.map { lhs =>
    operator(t => p1.contains(t)) match {
      case Some(t) =>
        val rhs = expect(e1, "e1 expression")
        BinaryExpression(lhs, t.toString, rhs)
      case None =>
        lhs
    }
  }

  // e.g. multiply/divide
  private def e2: Option[Expression] = e1.map { lhs =>
    operator(t => p2.contains(t)) match {
      case Some(t) =>
        val rhs = expect(e2, "e2 expression")
        BinaryExpression(lhs, t.toString, rhs)
      case None =>
        lhs
    }
  }

  // e.g. add/subtract
  private def e3: Option[Expression] = e2.map { lhs =>
    operator(t => p3.contains(t)) match {
      case Some(t) =>
        val rhs = expect(e3, "e3 expression")
        BinaryExpression(lhs, t.toString, rhs)
      case None =>
        lhs
    }
  }

  // e.g <, <=, ...
  private def e4: Option[Expression] = rollback {
    for {
      lhs <- e3
      rhs = rollback {
        for {
          op <- operator(t => p4.contains(t))
          rhs <- e4
        } yield (op.str, rhs)
      }
    } yield rhs
      .map { case (op, rhs) =>
        BinaryExpression(lhs, op, rhs)
      }
      .getOrElse(lhs)
    
  }

  // e.g. && ||
  private def e5: Option[Expression] = e4.map { lhs =>
    operator(t => p5.contains(t)) match {
      case Some(t) =>
        val rhs = expect(e5, "e5 expression")
        BinaryExpression(lhs, t.toString, rhs)
      case None =>
        lhs
    }
  }

  private def expression: Option[Expression] = rollback {
    e5
  }

  private def expressions: Seq[Expression] = expression match {
    case Some(e) =>
      operator(_ == ",") match {
        case Some(_) =>
          e +: expressions
        case None =>
          Seq(e)
      }
    case None =>
      Seq()
  }

  private def bitsType: Option[BitsType] =
    keyword("bits") match {
      case Some(_) =>
        if (operator(_ == "(").isEmpty)
          throw ParseError("expecting '('", lexer)
        val e = expect(expression, "expression")
        if (operator(_ == ")").isEmpty)
          throw ParseError("expecting ')'", lexer)
        Some(BitsType(e))
      case None =>
        keyword("bit") match {
          case Some(_) =>
            Some(BitsType(DecimalExpression("1")))
          case None =>
            None
        }
    }

  private def namedType: Option[NamedType] = for {
    id <- identifier(None)
  } yield NamedType(id)

  private def tupleType: Option[TupleType] = rollback {
    for {
      _ <- operator("(")
      ts = types
      _ <- operator(")")
    } yield TupleType(ts)
  }

  private def arrayType: Option[ArrayType] = for {
    _ <- keyword("array")
    _ <- operator("[")
    dim = repeat(expression)
    _ = expect("]")
    _ = expect("of")
    t = expect(`type`, "type")
  } yield ArrayType(dim, t)

  private def `type`: Option[Type] = {
    arrayType.orElse(tupleType).orElse(bitsType).orElse(namedType)
  }

  private def types: Seq[Type] = `type` match {
    case Some(t) =>
      lexer match {
        case Operator(",", _) #:: rest =>
          lexer = rest
          t +: types
        case _ =>
          Seq(t)
      }
    case _ =>
      Seq()
  }

  private def scopedName: Option[ScopedName] = lexer match {
    case (id: Identifier) #:: Operator(".", _) #:: Operator("<", _) #:: rest
        if !keywords.contains(id.str) =>
      lexer = rest
      val bits = ids
      val _ = expect(operator(">"), ">")
      Some(ScopedName(Seq(id), bits))
    case (id: Identifier) #:: Operator(".", _) #:: rest
        if !keywords.contains(id.str) =>
      lexer = rest
      scopedName match {
        case Some(sub) =>
          Some(ScopedName(id +: sub.ids, sub.bits))
        case None =>
          throw ParseError("expecting identifier", lexer)
      }
    case (id: Identifier) #:: rest if !keywords.contains(id.str) =>
      lexer = rest
      Some(ScopedName(Seq(id), Seq()))
    case _ =>
      None
  }

  private def formalArg: Option[FormalArg] = for {
    t <- `type`
    mod = operator("&")
    id = identifier().getOrElse(error("expecting <identifier>"))
  } yield FormalArg(t, mod, id)

  private def formalArgs: Seq[FormalArg] = {
    formalArg match {
      case Some(fa) =>
        operator(_ == ",") match {
          case Some(_) => fa +: formalArgs
          case None    => Seq(fa)
        }
      case None =>
        Seq()
    }
  }

  private def ids: Seq[Identifier] = identifier(None) match {
    case Some(id) =>
      operator(_ == ",") match {
        case Some(_) =>
          id +: ids
        case None =>
          Seq(id)
      }
    case None =>
      Seq()
  }

  private def assignment(): Option[Assignment] = rollback {
    for {
      lhs <- expression
      _ <- operator(_ == "=")
      rhs <- expression
    } yield Assignment(lhs, rhs)
  }

  private def declaration_modifier: Option[Identifier] = keyword("constant")

  private def declared_variable: Option[Seq[Identifier]] = {
    rollback {
      for {
        _ <- operator("(")
        ids = repeat(ignore.orElse(identifier()), ",")
        _ <- operator(")")
      } yield ids
    }
  }

  private def declaration(): Option[Declaration] = rollback {
    for {
      (mod, t, id) <- for {
        mod <- Some(repeat(declaration_modifier))
        t <- `type`
        id = repeat(identifier(), ",")
        if id.nonEmpty
      } yield (mod, t, id)

      init = for {
        _ <- operator("=")
        e = expect(expression, "expression")
      } yield e

    } yield Declaration(mod, t, id, init)
  }

  private def constant_declaration: Option[ConstDeclaration] = rollback {
    for {
      _ <- keyword("constant")
      id <- declared_variable
      _ <- operator("=")
      e = expect(expression, "expression")
    } yield ConstDeclaration(id, e)
  }

  private def top_declaration: Option[Declaration] = rollback {
    for {
      d <- declaration()
      _ <- operator(";")
    } yield d
  }

  private def else_if_phrase(): Option[(Expression, Statement)] = for {
    kw <- keyword("elsif")
    e = expect(expression, "expression")
    _ = expect(keyword("then"), "then")
    s <- body(kw.position).orElse(error("expecting <statement>"))
  } yield (e, Block(s))

  private def else_if_phrases(indent: Int): Seq[(Expression, Statement)] =
    else_if_phrase() match {
      case Some(p) =>
        p +: else_if_phrases(indent)
      case None =>
        Seq()
    }

  private def else_phrase(): Option[Statement] = for {
    kw <- keyword("else")
    s <- body(kw.position)
  } yield Block(s)

  private def return_statement(): Option[Return] = for {
    _ <- keyword("return")
    e = expression
  } yield Return(e)

  private def if_statement(indent: Int): Option[IfStatement] = for {
    kw <- keyword("if") // if
    c = expect(expression, "expression") // condition
    _ = expect(keyword("then"), "then") // then
    s <- body(kw.position) // statement
    ei = else_if_phrases(indent) // elsif*
    e = else_phrase()
  } yield IfStatement((c, Block(s)) +: ei, e)

  private def for_statement(): Option[ForStatement] = for {
    _ <- keyword("for")
    id = expect(identifier(), "id")
    _ = expect(operator("="), "=")
    start = expect(expression, "expression")
    kw = expect(keyword("to").orElse(keyword("downto")), "to|downto")
    end = expect(expression, "expression")
    b = expect(body(kw.position), "statement")
  } yield ForStatement(kw.str == "downto", id, start, end, Block(b))

  private def when_pattern = scopedName
    .orElse(bits_expression)
    .orElse(hex_expression)
    .orElse(decimalExpression)

  private def when_statement(): Option[WhenStatement] = for {
    kw <- keyword("when")
    c = repeat(when_pattern, ",")
    s = expect(body(kw.position), "statement")
  } yield WhenStatement(c, Block(s))

  private def otherwise(indent: Int): Option[Statement] = lexer match {
    case Identifier("otherwise", pos) #:: rest if pos.indent > indent =>
      lexer = rest
      Some(expect(body(pos).map(Block.apply), "statement"))
    case _ =>
      None
  }

  private def case_statement(indent: Int): Option[CaseStatement] = for {
    _ <- keyword("case")
    e = expect(expression, "expression")
    _ = expect(keyword("of"), "of")
    b = expect(indented(indent, _ => when_statement()), "when ...")
    o = otherwise(indent)
  } yield CaseStatement(e, b, o)

  private def call_statement(): Option[CallStatement] = rollback {
    for {
      sn <- scopedName
      _ <- operator("(")
      args = repeat(expression, ",")
      _ <- operator(")")
    } yield CallStatement(sn, args)
  }

  private def repeat_statement(): Option[RepeatStatement] = for {
    kw <- keyword("repeat")
    b <- body(kw.position)
    _ <- keyword("until")
    u = expect(expression, "expression")
  } yield RepeatStatement(Block(b), u)

  private def while_statement(): Option[WhileStatement] = for {
    kw <- keyword("while")
    c = expect(expression, "expression")
    _ = expect(keyword("do"), "do")
    b = expect(body(kw.position), "body")
  } yield WhileStatement(c, Block(b))

  private def assert_statement(): Option[AssertStatement] = for {
    _ <- keyword("assert")
    e = expect(expression, "expression")
  } yield AssertStatement(e)

  private def see_statement: Option[SeeStatement] = lexer match {
    case Identifier("SEE", _) #:: Str(s, _) #:: rest =>
      lexer = rest
      Some(SeeStatement(s))
    case _ =>
      None
  }

  private def undefined: Option[Undefined.type] = for {
    _ <- keyword("UNDEFINED")
  } yield Undefined

  private def unpredictable: Option[Unpredictable.type] = for {
    _ <- keyword("UNPREDICTABLE")
  } yield Unpredictable

  def statement(indent: Int): Option[Statement] = {
    lexer.headOption match {
      case Some(t) =>
        if (t.position.indent == indent)
          for {
            s <- return_statement()
              .orElse(constant_declaration)
              .orElse(repeat_statement())
              .orElse(while_statement())
              .orElse(undefined)
              .orElse(see_statement)
              .orElse(unpredictable)
              .orElse(case_statement(indent))
              .orElse(for_statement())
              .orElse(assert_statement())
              .orElse(if_statement(indent))
              .orElse(call_statement())
              .orElse(declaration())
              .orElse(assignment())
              .orElse(implementation_defined)
            _ = if (s.isSimple) expect(operator(_ == ";"), ";") else ()
          } yield s
        else if (t.position.indent < indent)
          None
        else
          error(s"illegal indentation ${t.position.indent} > $indent")
      case None =>
        None
    }
  }

  private def indented[A](indent: Int, f: Int => Option[A]): Option[Seq[A]] =
    lexer.headOption match {
      case Some(t) if t.position.indent > indent =>
        Some(repeat(f(t.position.indent)))
      case _ =>
        None
    }

  private def same_line(pos: Position): Seq[Statement] =
    lexer.headOption match {
      case Some(t) if t.position.row == pos.row =>
        statement(pos.indent) match {
          case Some(s) =>
            s +: same_line(pos)
          case None =>
            Seq()
        }
      case _ =>
        Seq()
    }

  private def body[A](pos: Position): Option[Seq[Statement]] =
    indented(pos.indent, statement).orElse(Some(same_line(pos)))

  // def statement_or_block(indent: Int): Option[Statement] =
  //  block(indent, statement).map(Block.apply).orElse(statement(indent))

  private def function_decl_start
      : Option[(Option[Type], ScopedName, Position)] =
    rollback {
      def with_type = rollback {
        for {
          t <- `type`
          sn <- scopedName
          lp <- operator(_ == "(")
        } yield (Some(t), sn, lp.position)
      }

      def without_type = rollback {
        for {
          sn <- scopedName
          lp <- operator(_ == "(")
        } yield (None, sn, lp.position)
      }

      with_type.orElse(without_type)
    }

  private def functionDecl: Option[FunctionDecl] = rollback {
    for {
      (t, sn, _) <- function_decl_start
      a = formalArgs
      _ = operator(_ == ")").getOrElse(throw ParseError("expecting )", lexer))
      _ <- operator(";")
    } yield FunctionDecl(t, sn, a)
  }

  private def functionDef: Option[FunctionDef] = rollback {
    for {
      (t, sn, pos) <- function_decl_start
      a = formalArgs
      _ = operator(_ == ")").getOrElse(throw ParseError("expecting )", lexer))
      b <- body(pos)
    } yield FunctionDef(t, sn, a, Block(b))
  }

  private def arrayDeclaration: Option[ArrayDeclaration] = rollback {
    for {
      _ <- keyword("array")
      t <- `type`
      sn <- scopedName
      _ = expect("[")
      dim = expect(expression, "expression")
      _ = expect("]")
      _ = expect(";")
    } yield ArrayDeclaration(t, sn, dim)
  }

  private def arrayVarDeclaration: Option[ArrayDeclaration] = rollback {
    for {
      _ <- keyword("array")
      _ <- operator("[")
      dim = expect(expression, "expression")
      _ = expect("]")
      _ = expect("of")
      t = expect(`type`, "type")
      sn = expect(scopedName, "name")
      _ = expect(";")
    } yield ArrayDeclaration(t, sn, dim)
  }

  private def arrayGetterDef: Option[ArrayGetter] = rollback {
    for {
      t <- `type`
      sn <- scopedName
      kw <- operator("[")
      a = formalArgs
      _ = expect("]")
      b <- operator(";").map(_ => Seq()).orElse(body(kw.position))
    } yield ArrayGetter(t, sn, a, Block(b))
  }

  private def arraySetterDef: Option[ArraySetter] = rollback {
    for {
      sn <- scopedName
      kw <- operator("[")
      a = formalArgs
      _ = expect("]")
      _ = expect("=")
      rhs = expect(formalArg, "formal arg")
      b <- operator(";").map(_ => Seq()).orElse(body(kw.position))
    } yield ArraySetter(sn, a, rhs, Block(b))
  }

  private def regGetter: Option[RegGetter] = rollback {
    lexer.headOption.flatMap { token =>
      for {
        t <- `type`
        sn <- scopedName
        b <- body(token.position)
      } yield RegGetter(t, sn, Block(b))
    }
  }

  private def regSetter: Option[RegSetter] = rollback {
    for {
      sn <- scopedName
      kw <- operator("=")
      rhs <- formalArg
      b <- body(kw.position)
    } yield RegSetter(sn, rhs, Block(b))
  }

  private def enum_declaration = for {
    _ <- keyword("enumeration")
    id = expect(identifier(), "identifier")
    _ = expect(operator("{"), "{")
    choices = repeat(identifier(), ",")
    _ = expect(operator("}"), "}")
    _ = expect(operator(";"), ";")
  } yield EnumDeclaration(id, choices)

  private def memberDeclaration = for {
    t <- `type`
    id <- identifier()
  } yield MemberDeclaration(t, id)

  private def structDeclaration: Option[StructDeclaration] = rollback {
    for {
      _ <- keyword("type")
      id <- identifier()
      _ <- keyword("is")
      _ = expect("(")
      members = repeat(memberDeclaration, ",")
      _ = expect(")")
    } yield StructDeclaration(id, members)
  }

  private def typeDeclaration: Option[TypeDeclaration] = rollback {
    for {
      _ <- keyword("type")
      id <- identifier()
      t = for {
        _ <- operator("=")
        t = expect(`type`, "type")
      } yield t
      _ <- operator(";")
    } yield TypeDeclaration(id, t)
  }

  def topLevel: Seq[TopLevel] = {
    val out = repeat(
      enum_declaration
        .orElse(structDeclaration)
        .orElse(typeDeclaration)
        .orElse(arrayDeclaration)
        .orElse(arrayVarDeclaration)
        .orElse(top_declaration)
        .orElse(functionDecl)
        .orElse(functionDef)
        .orElse(arrayGetterDef)
        .orElse(arraySetterDef)
        .orElse(regSetter)
        .orElse(regGetter)
        .orElse(top_declaration)
    )
    if (lexer.nonEmpty) {
      error("extra lines")
    }
    out
  }

}
