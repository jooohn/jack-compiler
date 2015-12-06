package jackcompiler.syntax

import jackcompiler.exception.ParseException
import jackcompiler.{Node, Token}

trait ControlSyntax extends Syntax with ParentSyntax

case class Repeat(children: Syntax*) extends ControlSyntax {

  override def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token]) = try {
    val (parsedNodes, remainingTokens) = parseChildren(tokens)
    val (nextNodes, nextRemainingTokens) = parse(remainingTokens)
    (parsedNodes ++ nextNodes, nextRemainingTokens)
  } catch {
    case e: ParseException => (Nil, tokens)
  }

}

case class Or(children: Syntax*) extends ControlSyntax {

  override def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token]) = {
    children.foldLeft[Option[(Seq[Node], Seq[Token])]](None) { (maybe, syntax) =>
      maybe orElse {
        try {
          Some(syntax.parse(tokens))
        } catch {
          case e: ParseException => None
        }
      }
    } match {
      case Some(ret) => ret
      case _ => fail(s"No candidate is matched as OR syntax. [${children.map(_.getClass).mkString(",")}]", tokens)
    }
  }

}

case class Maybe(children: Syntax*) extends ControlSyntax {

  override def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token]) = {
    try {
      val ret = parseChildren(tokens)
      ret
    } catch {
      case e: ParseException => {
        (Nil, tokens)
      }
      case e: Throwable =>
        fail(e.getMessage, tokens)
    }
  }

}
case class Group(children: Syntax*) extends ControlSyntax {

  override def parse(tokens: Seq[Token]): (Seq[Node], Seq[Token]) = parseChildren(tokens)

}
