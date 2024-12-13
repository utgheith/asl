package ag.asl

import ag.asl.parser.Instruction
import ag.rules.{Maker, Rule, SignedPath, run, say}

import scala.collection.{SortedMap, SortedSet}
import scala.xml.factory.XMLLoader
import scala.xml.{Elem, SAXParser}

object Spec {
  val spec_url: Maker[String] = Rule() {
    "https://developer.arm.com/-/media/developer/products/architecture/armv9-a-architecture/2023-09/ISA_A64_xml_A_profile-2023-09.tar.gz?rev=9929e0d319dc475eaeffa29fc1e0a996&hash=E4B5C4F2B29E744772E001CAC461168E"
  }

  val raw_spec: Maker[SignedPath[?]] =
    SignedPath.rule(spec_url, SortedSet(), null) { case (dir, spec_url) =>
      say(s":::::::::::::::::::::: $dir")
      os.makeDir(dir)
      val saved_spec = os.home / s"saved_spec_${spec_url.hashCode().toString}"
      if (os.exists(saved_spec)) {
        say("getting cached spec")
        os.copy(saved_spec, dir / "spec.tgz")
      } else {
        say("downloading spec")
        val _ = os.proc("wget", "-O", dir / "spec.tgz", spec_url).run()
        os.copy.over(dir / "spec.tgz", saved_spec)
      }
      val _ = os.proc("tar", "xvfz", "spec.tgz").run(cwd = dir)
    }

  private lazy val loader: XMLLoader[Elem] = new XMLLoader[Elem] {
    override def parser: SAXParser = {
      import scala.language.unsafeNulls
      val f = javax.xml.parsers.SAXParserFactory.newInstance()
      f.setNamespaceAware(false)
      f.setFeature(
        "http://apache.org/xml/features/disallow-doctype-decl",
        false
      );
      f.newSAXParser()
    }
  }

  val shared_functions: Maker[SortedMap[String, String]] =
    Rule(raw_spec, null) { raw_spec =>
      val dir = raw_spec.path / "ISA_A64_xml_A_profile-2023-09"

      val ps: xml.NodeSeq =
        loader.loadFile((dir / "shared_pseudocode.xml").toIO) \\ "ps"

      val s: Seq[(String, String)] = for {
        n <- ps
        name = n \@ "name"
        text = (n \ "pstext").text
      } yield (name, text)

      SortedMap(s*)
    }

  val instructions: Maker[SortedMap[String, Instruction]] =
    Rule(raw_spec, null) { raw_spec =>
      val dir = raw_spec.path / "ISA_A64_xml_A_profile-2023-09"

      val instruction_sections: xml.NodeSeq =
        loader.loadFile((dir / "onebigfile.xml").toIO) \\ "instructionsection"

      val s = for {
        node <- instruction_sections
        typ = node.attribute("type").map(_.toString)
        if typ.contains("instruction")
      } yield Instruction(node)

      s.map(i => (i.id, i)).to(SortedMap)

    }
  /*
  def instruction_logic(name: String): Maker[Instruction] = Rule(instructions, name) { instructions =>
    instructions(name)
  }

  val instruction_names: Maker[SortedSet[String]] = Rule(instructions) { instructions =>
    instructions.keySet
  }

   */
}
