package jackcompiler.syntax

import jackcompiler.{ParentNode, Node, Token}

trait NonTerminalSyntax extends Syntax with ParentSyntax {

  val name: String = {
    val className = getClass.getSimpleName
    className.head.toLower + className.tail.init
  }

  override def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token]) = {
    val (childNodes, remainingTokens) = parseChildren(tokens)
    val parent = ParentNode(name, childNodes)
    (Seq(parent), remainingTokens)
  }

}

case object Class extends NonTerminalSyntax {
  override val children = Seq(
    Keyword("class"),
    ClassName,
    Symbol("{"),
    Repeat(ClassVarDec),
    Repeat(SubroutineDec),
    Symbol("}")
  )
}

case object ClassVarDec extends NonTerminalSyntax {
  override val children = Seq(
    Or(Keyword("static"), Keyword("field")),
    Type,
    VarName, Repeat(Symbol(","), VarName),
    Symbol(";")
  )
}

case object SubroutineDec extends NonTerminalSyntax {
  override val children = Seq(
    Or(Keyword("constructor"), Keyword("function"), Keyword("method")),
    Or(Keyword("void"), Type),
    SubroutineName,
    Symbol("("), ParameterList, Symbol(")"),
    SubroutineBody
  )
}

case object SubroutineBody extends NonTerminalSyntax {
  override val children = Seq(
    Symbol("{"), Repeat(VarDec), Statements, Symbol("}")
  )
}

case object ParameterList extends NonTerminalSyntax {
  override val children = Seq(
    Maybe(
      Type,
      VarName,
      Repeat(Symbol(","), Type, VarName)
    )
  )
}

case object VarDec extends NonTerminalSyntax {
  override val children = Seq(
    Keyword("var"),
    Type,
    VarName,
    Repeat(Symbol(","), VarName),
    Symbol(";")
  )
}

case object Statements extends NonTerminalSyntax {
  override val children = Seq(Repeat(Statement))
}

case object LetStatement extends NonTerminalSyntax {
  override val children = Seq(
    Keyword("let"),
    VarName,
    Maybe(Symbol("["), Expression, Symbol("]")),
    Symbol("="),
    Expression,
    Symbol(";")
  )
}

case object IfStatement extends NonTerminalSyntax {
  override val children = Seq(
    Keyword("if"),
    Symbol("("), Expression, Symbol(")"),
    Symbol("{"), Statements, Symbol("}"),
    Maybe(
      Keyword("else"),
      Symbol("{"), Statements, Symbol("}")
    )
  )
}

case object WhileStatement extends NonTerminalSyntax {
  override val children = Seq(
    Keyword("while"),
    Symbol("("), Expression, Symbol(")"),
    Symbol("{"), Statements, Symbol("}")
  )
}

case object DoStatement extends NonTerminalSyntax {
  override val children = Seq(Keyword("do"), SubroutineCall, Symbol(";"))
}

case object ReturnStatement extends NonTerminalSyntax {
  override val children = Seq(
    Keyword("return"), Maybe(Expression), Symbol(";")
  )
}

case object Expression extends NonTerminalSyntax {
  override val children = Seq(Term, Repeat(Op, Term))
}

case object Term extends NonTerminalSyntax {
  override val children = Seq(
    Or(
      IntegerConstant,
      StringConstant,
      KeywordConstant,
      SubroutineCall,
      Group(VarName, Symbol("["), Expression, Symbol("]")),
      VarName,
      Group(Symbol("("), Expression, Symbol(")")),
      Group(UnaryOp, Term)
    )
  )
}

case object ExpressionList extends NonTerminalSyntax {
  override val children = Seq(
    Maybe(Expression, Repeat(Symbol(","), Expression))
  )
}
