package jackcompiler

import java.io.{PrintWriter, File}

class Engine {

  def compile(tokenizer: Tokenizer, out: File) = {
    val (nodes, remainingTokens) = syntax.Class.parse(tokenizer.tokenize)
    if (remainingTokens.nonEmpty) {
      throw new RuntimeException("remaining tokens exist.")
    }

    val writer = new PrintWriter(out)
    nodes.foreach(_.out(writer, 0))
    writer.close()
  }

}
