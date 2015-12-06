package jackcompiler.syntax

import jackcompiler._
import jackcompiler.exception.ParseException

trait Syntax {

  def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token])

  protected def fail(message: String, remainingTokens: Seq[Token]) = {
    throw new ParseException(message)
  }

}

trait ParentSyntax {

  val children: Seq[Syntax]

  def parseChildren(tokens: Seq[Token]): (Seq[Node], Seq[Token]) = {
    children.foldLeft[(Seq[Node], Seq[Token])]((Nil, tokens)) {
      case ((parsedNodes, remainingTokens), syntax) => {
        val (eachNodes, eachRemainingTokens) = syntax.parse(remainingTokens)
        (parsedNodes ++ eachNodes, eachRemainingTokens)
      }
    }
  }

}
