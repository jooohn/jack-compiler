package jackcompiler

import java.io.File

import scala.io.Source

class Tokenizer(file: File) {

  def tokenize: Seq[Token] = {
    val source = Source.fromFile(file)
    try {
      val contents = source.getLines()
        .map(_.replaceFirst("""//.*$""", ""))
        .mkString
        .replaceAll("""\Q/*\E.*?\Q*/\E""", " ")
      tokenize(Nil, contents)
    } finally {
      source.close()
    }
  }

  private val tokenizationRules: Seq[TokenizationRule] = Seq(
    TokenizationRule.Symbol,
    TokenizationRule.Keyword,
    TokenizationRule.IntegerConstant,
    TokenizationRule.StringConstant,
    TokenizationRule.Identifier
  )

  private def tokenize(tokens: Seq[Token], remainder: String): Seq[Token] = {
    val trimmed = remainder.replaceFirst("""^\s+""", "")
    if (trimmed.isEmpty) {
      tokens
    } else {
      tokenizationRules.foldLeft[Option[(Token, Int)]](None)((maybeToken, rule) => {
        maybeToken orElse {
          rule.regex.findFirstIn(trimmed) map { s =>
            (rule.toToken(s), s.length)
          }
        }
      }) match {
        case Some((token, length)) =>
          tokenize(tokens :+ token, trimmed.substring(length))
        case _ =>
          throw new RuntimeException(s"""no token is matched for "$trimmed".""")
      }
    }
  }

}
