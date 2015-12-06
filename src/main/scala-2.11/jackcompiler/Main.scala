package jackcompiler

import java.io.{FilenameFilter, FileNotFoundException, File}

object Main extends App {
  val file = new File(args.head)
  val files: List[File] = if (file.isFile) {
    List(file)
  } else if (file.isDirectory) {
    file.listFiles(JackFileFilter).toList
  } else {
    throw new FileNotFoundException(file.getAbsolutePath)
  }

  files.foreach { f =>
    val tokenizer = new Tokenizer(f)
    val dir = f.getParentFile
    val name = f.getName.replaceAll("""\.\S+$""", ".xml.out")
    val target = new File(List(dir.getAbsolutePath, name).mkString(File.separator))
    val engine = new Engine
    engine.compile(tokenizer, target)
  }

  object JackFileFilter extends FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name.endsWith(".jack")
  }
}
