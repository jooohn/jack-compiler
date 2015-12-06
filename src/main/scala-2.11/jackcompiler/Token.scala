package jackcompiler

import java.io.PrintWriter

abstract class Token(val token: String) extends Node {

  val tag: String

  def value: String = token

  override def out(writer: PrintWriter, indent: Int): Unit = {
    writer.write("  " * indent)
    writer.println(s"<$tag> $value </$tag>")
  }

}

case class KeywordToken(val keyword: String) extends Token(keyword) {
  val tag = "keyword"
}

case class SymbolToken(val symbol: String) extends Token(symbol) {
  val tag = "symbol"

  override val value: String = symbol match {
    case "<" => "&lt;"
    case ">" => "&gt;"
    case "&" => "&amp;"
    case _ => symbol
  }
}

case class IntegerConstantToken(val integer: Int) extends Token(integer.toString) {
  val tag = "integerConstant"
}

case class StringConstantToken(val string: String) extends Token(string) {
  val tag = "stringConstant"
}

case class IdentifierToken(val identifier: String) extends Token(identifier) {
  val tag = "identifier"
}
