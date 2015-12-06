package jackcompiler

import java.io.PrintWriter

trait Node {

  def out(writer: PrintWriter, indent: Int): Unit

}

case class ParentNode(val name: String, val children: Seq[Node]) extends Node {

  def out(writer: PrintWriter, indent: Int): Unit = {
    writer.write("  " * indent)
    writer.println(s"<${name}>")
    children.foreach(_.out(writer, indent + 1))
    writer.write("  " * indent)
    writer.println(s"</${name}>")
  }

}
