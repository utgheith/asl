package ag.asl

import ag.asl.parser.{Lexer, Parser}
import ag.rules.State
import upickle.default.{ReadWriter, write}

var i: Int = 1

val out_dir = {
  val d = os.pwd / "generated_asl"
  // os.makeDir(d)
  d
}

def compile[A: ReadWriter](
    name: String,
    code: String,
    goal: Parser => A
): Unit = {
  println(s"[$i] $name")
  // println(code)
  val lexer = Lexer.from(code)
  // println(lexer.toList)
  val parser = Parser(lexer)
  try {
    val out = goal(parser) // .topLevel

    val out_path = out_dir / os.RelPath(s"${name}_$i")
    os.write.over(out_path, code, createFolders = true)
    os.write.append(out_path, "\n\n\n")
    os.write.append(out_path, write(out, indent = 2))

  } catch {
    case ex: Throwable =>
      println(code)
      parser.lexer.headOption foreach { t =>
        val row = t.position.row
        val lines = code.split('\n').toSeq.zipWithIndex
        lines.foreach { case (line, num) =>
          println(line)
          if (num == row) {
            val spaces = "." * t.position.column
            println(s"$spaces^")
            println(s"$spaces| ${ex.getMessage}")
          }
        }
      }
      println(s"***** $name failed after $i")
      throw ex
  }
  i += 1
}

@main def main(): Unit = {
  given State = State.of(os.pwd / "workspace")
  val shared = Spec.shared_functions.value

  for {
    (name, code) <- shared
  } {
    compile(name, code, _.topLevel)
  }

  for {
    (_, ins) <- Spec.instructions.value // .slice(0,20)
  } {
    // pprint.pprintln(ins)
    println(ins.id)
    for {
      iclass <- ins.iclasses
    } {
      println(s"    ${iclass.name} ${iclass.id}")
      for {
        reg_diagram <- iclass.reg_diagrams
      } {
        println(f"        ${reg_diagram.mask}%08x ${reg_diagram.value}%08x")
        for (pst <- iclass.ps_text) {
          println(pst.section)
          compile(
            s"${ins.id}_${pst.section}",
            pst.code,
            p => p.repeat(p.statement(0))
          )
        }
        // println(ins.ps_text)
      }
    }

    // ins.ps_text.toSeq.foreach(println)

    for (pst <- ins.ps_text) {
      println(pst.section)
      compile(
        s"${ins.id}_${pst.section}",
        pst.code,
        p => p.repeat(p.statement(0))
      )
    }
  }

}
