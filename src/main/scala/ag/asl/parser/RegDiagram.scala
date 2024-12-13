package ag.asl.parser

import upickle.default.ReadWriter

import scala.collection.{SortedMap, mutable}
import scala.util.control.NonFatal

case class DocVars(map: SortedMap[String, String]) derives ReadWriter
object DocVars {
  def apply(node: xml.NodeSeq): DocVars = {
    val s = for {
      dvn <- node \ "docvar"
    } yield (dvn \@ "key", dvn \@ "value")

    DocVars(s.to(SortedMap))
  }
}

///////////// BitPattern ///////////

case class BitPattern(
    width: Int,
    mask: Int,
    value: Int,
    excluded: Option[BitPattern]
) derives ReadWriter

object BitPattern {

  private val simple_cache_ : mutable.Map[String, BitPattern] = mutable.Map()

  private def parseInt(s: String): Int = if (s.isEmpty) {
    0
  } else {
    Integer.parseInt(s, 2).nn
  }

  def apply(s: String): BitPattern = try {
    if (s.startsWith("!= ")) {
      import language.unsafeNulls
      val base = s.substring(3)
      BitPattern(base).copy(excluded = Some(BitPattern(base)))
    } else {
      simple_cache_.getOrElseUpdate(
        s, {
          import language.unsafeNulls
          val t =
            if (s.isEmpty) "x"
            else s.replaceAll("""\(""", "").replaceAll("""\)""", "")
          val mask = parseInt(t.map {
            case 'x' => '0'
            case 'N' => '0'
            case 'z' => '1'
            case 'Z' => '1'
            case _   => '1'
          })
          val value = parseInt(t.map {
            case 'x' => '0'
            case 'N' => '0'
            case 'z' => '0'
            case 'Z' => '0'
            case x   => x
          })

          BitPattern(
            width = t.length,
            mask = mask,
            value = value,
            excluded = None
          )
        }
      )
    }
  } catch {
    case NonFatal(ex) =>
      throw IllegalArgumentException(s, ex)
  }

}

////////////// Column ///////////

case class Column(span: Int, pattern: String, bit_pattern: BitPattern)
    derives ReadWriter {}
object Column {
  def apply(node: xml.Node): Column = {
    val pattern = node.text
    val bit_pattern = BitPattern(pattern)
    Column(
      span = node \@ "colspan" match {
        case "" => bit_pattern.width
        case x  => x.toInt
      },
      pattern = pattern,
      bit_pattern = bit_pattern
    )
  }

}

////////// Box ///////////////

case class Box(
    name: Option[String],
    hibit: Int,
    width: Int,
    settings: Int,
    use_name: Boolean,
    columns: Seq[Column],
    mask: Int,
    value: Int
) derives ReadWriter {}

object Box {

  def apply(node: xml.Node): Box = {

    def asInt(name: String, d: => Int): Int = node \@ name match {
      case "" => d
      case x  => x.toInt
    }

    val columns = for {
      column_node <- node \ "c"
    } yield Column(column_node)

    val (computed_width, mask, value) = columns.reverse.foldLeft((0, 0, 0)) {
      case ((shift, mask, value), c) =>
        (
          (shift + c.bit_pattern.width),
          mask | (c.bit_pattern.mask << shift),
          value | (c.bit_pattern.value << shift)
        )
    }

    val width = asInt("width", 1)

    println(s"******* $width $computed_width")
    // assert(width == computed_width)

    Box(
      name = node \@ "name" match {
        case "" => None
        case x  => Some(x)
      },
      hibit = asInt("hibit", throw new Exception(s"hibit is missing in $node")),
      width = width,
      settings = asInt("settings", 0),
      use_name = node \@ "usename" match {
        case "1" => true
        case _   => false
      },
      columns = columns,
      mask = mask,
      value = value
    )
  }
}

case class RegDiagram(form: String, boxes: Seq[Box], mask: Int, value: Int)
    derives ReadWriter {}

object RegDiagram {
  def apply(node: xml.Node): RegDiagram = {
    val boxes = for {
      xml_box <- (node \ "box")
    } yield Box(xml_box)

    val (mask, value) = boxes.foldLeft((0, 0)) { case ((mask, value), b) =>
      val shift = b.hibit - b.width + 1
      (mask | (b.mask << shift), value | (b.value << shift))
    }

    RegDiagram(
      form = node \@ "form",
      boxes = boxes,
      mask = mask,
      value = value
    )
  }
}

/////////////////// Encoding //////////////////

case class Encoding(doc_vars: DocVars, asm_template: String, boxes: Seq[Box])
    derives ReadWriter
object Encoding {
  def apply(node: xml.Node): Encoding = Encoding(
    doc_vars = DocVars(node \ "docvars"),
    asm_template = (node \ "asmtemplate").text,
    boxes = for {
      box_xml <- node \ "box"
    } yield Box(box_xml)
  )
}

/////////////////// ArchVariant /////////////

case class ArchVariant(name: String, feature: String) derives ReadWriter
object ArchVariant {
  def apply(node: xml.Node): ArchVariant = ArchVariant(
    node \@ "name",
    node \@ "feature"
  )
}

/////////////// PsText ///////////////

case class PsText(section: String, code: String) derives ReadWriter
object PsText {
  def apply(node: xml.Node): PsText = PsText(
    section = node \@ "section",
    code = (node \ "line").map(_.text).mkString("\n")
  )
}

/////////////////// IClass ///////////

case class IClass(
    name: String,
    id: String,
    isa: String,
    arch_variants: Seq[ArchVariant],
    reg_diagrams: Seq[RegDiagram],
    encodings: Seq[Encoding],
    ps_text: Seq[PsText]
) derives ReadWriter
object IClass {
  def apply(node: xml.Node): IClass = {
    val arch_variants = for {
      arch_variant_xml <- node \ "arch_variants" \ "arch_variant"
    } yield ArchVariant(arch_variant_xml)

    val reg_diagrams = for {
      reg_diagram_xml <- node \ "regdiagram"
    } yield RegDiagram(reg_diagram_xml)

    val encodings = for {
      encoding_xml <- node \ "encoding"
    } yield Encoding(encoding_xml)

    val ps_text = for {
      ps_text_xml <- node \ "ps_section" \ "ps" \ "pstext"
    } yield PsText(ps_text_xml)

    IClass(
      node \@ "name",
      node \@ "id",
      node \@ "isa",
      arch_variants,
      reg_diagrams,
      encodings,
      ps_text
    )
  }
}

/////////////////// Instruction /////////////////

case class Instruction(
    id: String,
    iclasses: Seq[IClass],
    doc_vars: DocVars,
    ps_text: Seq[PsText]
) derives ReadWriter

object Instruction {
  def apply(node: xml.Node): Instruction = {

    val id = node \@ "id"

    println(s"**** $id")

    val iclasses = for {
      iclass_xml <- node \ "classes" \ "iclass"
    } yield IClass(iclass_xml)

    val ps_text = for {
      ps_text_xml <- node \ "ps_section" \ "ps" \ "pstext"
    } yield PsText(ps_text_xml)

    Instruction(
      id = id,
      iclasses,
      DocVars(node \ "docvars"),
      ps_text = ps_text
    )

  }
}
