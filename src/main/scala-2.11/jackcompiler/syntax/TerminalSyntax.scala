package jackcompiler.syntax

import jackcompiler._

import scala.reflect.ClassTag

abstract class SpecificTerminalSyntax[T <: Token : ClassTag] extends Syntax {

  val expected: String

  override def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token]) = {
    val klass = implicitly[ClassTag[T]].runtimeClass
    val token = tokens.head
    val message = s"$expected is expected, but ${token.token} is given."

    token match {
      case t if klass.isInstance(t) =>
        if (t.token == expected) {
          (Seq(t), tokens.tail)
        } else {
          fail(message, tokens)
        }
      case _ => fail(message, tokens)
    }
  }

}

abstract class AnyTerminalSyntax[T <: Token : ClassTag] extends Syntax {

  override def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token]) = {
    val klass = implicitly[ClassTag[T]].runtimeClass
    val token = tokens.head
    token match {
      case t if klass.isInstance(t) => {
        (Seq(t), tokens.tail)
      }
      case _ => fail(s"${token.token} is given.", tokens)
    }
  }

}

case class Keyword(expected: String) extends SpecificTerminalSyntax[KeywordToken]
case class Symbol(expected: String) extends SpecificTerminalSyntax[SymbolToken]
case object IntegerConstant extends AnyTerminalSyntax[IntegerConstantToken]
case object StringConstant extends AnyTerminalSyntax[StringConstantToken]
case object Identifier extends AnyTerminalSyntax[IdentifierToken]
