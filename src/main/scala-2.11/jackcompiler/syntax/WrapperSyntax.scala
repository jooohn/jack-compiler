package jackcompiler.syntax

import jackcompiler.{Token, Node}

trait WrapperSyntax extends Syntax with ParentSyntax {

  override def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token]) =
    parseChildren(tokens)
}

case object Type extends WrapperSyntax {
  override val children = Seq(
    Or(
      Keyword("int"),
      Keyword("char"),
      Keyword("boolean"),
      ClassName
    )
  )
}

case object Statement extends WrapperSyntax {
  override val children = Seq(
    Or(
      LetStatement,
      IfStatement,
      WhileStatement,
      DoStatement,
      ReturnStatement
    )
  )
}

case object SubroutineCall extends WrapperSyntax {
  override val children = Seq(
    Or(
      Group(SubroutineName, Symbol("("), ExpressionList, Symbol(")")),
      Group(
        Or(ClassName, VarName), Symbol("."), SubroutineName,
        Symbol("("), ExpressionList, Symbol(")")
      )
    )
  )
}

case object Op extends WrapperSyntax {
  override val children = Seq(
    Or(
      Symbol("+"),
      Symbol("-"),
      Symbol("*"),
      Symbol("/"),
      Symbol("&"),
      Symbol("|"),
      Symbol("<"),
      Symbol(">"),
      Symbol("=")
    )
  )
}

case object UnaryOp extends WrapperSyntax {
  override val children = Seq(
    Or(
      Symbol("-"),
      Symbol("~")
    )
  )
}

case object KeywordConstant extends WrapperSyntax {
  override val children = Seq(
    Or(
      Keyword("true"),
      Keyword("false"),
      Keyword("null"),
      Keyword("this")
    )
  )
}

trait IdentifierSyntax extends Syntax {
  override def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token]) =
    Identifier.parse(tokens)
}
case object ClassName extends WrapperSyntax {
  val children = Seq(Identifier)
}
case object SubroutineName extends WrapperSyntax {
  val children = Seq(Identifier)
}
case object VarName extends IdentifierSyntax {
  val children = Seq(Identifier)
}
