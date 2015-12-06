package jackcompiler

import scala.util.matching.Regex

sealed abstract class TokenizationRule(val regex: Regex, val toToken: (String) => Token)
object TokenizationRule {

  case object Keyword extends TokenizationRule(
    """\A(class|constructor|function|method|field|static|var|int|char|boolean|void|true|false|null|this|let|do|if|else|while|return)""".r, KeywordToken)

  case object Symbol extends TokenizationRule(
    """\A[{}()\[\].,;+\-*/&|<>=~]""".r, SymbolToken)

  case object IntegerConstant extends TokenizationRule(
    """\A\d+""".r,
    t => IntegerConstantToken(t.toInt))

  case object StringConstant extends TokenizationRule(
    """\A\"[^"]*?\"""".r,
    t => StringConstantToken(t.substring(1, t.length - 1)))

  case object Identifier extends TokenizationRule("""\A[a-zA-Z_][0-9a-zA-Z_]*""".r, IdentifierToken)
}
