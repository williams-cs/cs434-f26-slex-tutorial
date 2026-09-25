package scup

import java.io.File
import java.nio.file.Files

/**
 * Build-time spec-file source generation (Path A): wired into
 * build.sbt as a `Compile / sourceGenerators` task, so the synthetic
 * actions objects are compiled by the normal build (and re-checked by
 * Metals on every save), with diagnostics at real spec lines.
 *
 * Compiled by the sbt metabuild (Scala 2.12) together with the
 * verbatim metabuild copy of SpecAssemble.scala.
 */
object SpecGen {

  /** Generate one actions object per `*.slex` in `baseDir`. */
  def generateSlex(baseDir: File, outDir: File): Seq[File] =
    generateAll(baseDir, outDir).filterNot(_.getName.endsWith("_scup.scala"))

  /** A user error in a spec file aborts the build with just its
   *  message: sbt prints a full stack trace for any other exception
   *  thrown from a task, which buries the path:line: diagnostic the
   *  spec author actually needs. */
  private def fail(msg: String): Nothing = throw new sbt.MessageOnlyException(msg)

  /** Generate actions objects for every `*.slex` and `*.scup` in
   *  `baseDir`.  Grammar specs type their terminals with the kind
   *  enum of the (single) companion `.slex`. */
  def generateAll(baseDir: File, outDir: File): Seq[File] = try {
    def specs(ext: String) = Option(baseDir.listFiles).getOrElse(Array[File]())
      .filter(_.getName.endsWith(ext)).sortBy(_.getName).toSeq
    def read(f: File) = new String(Files.readAllBytes(f.toPath), "UTF-8")
    val lexModels = specs(".slex").map(f => SpecAssemble.parseLex(f.getName, read(f)))
    val lexOut = lexModels.map { m =>
      val kinds = m.kinds.getOrElse(
        fail(m.path + ": %kinds declaration is required for build-time generation"))
      val out = new File(outDir, m.objectName + ".scala")
      writeIfChanged(out, SpecAssemble.assembleLex(m, kinds))
      out
    }
    val scupOut = specs(".scup").map { f =>
      val m = SpecAssemble.parseScup(f.getName, read(f))
      val kinds = lexModels.flatMap(_.kinds.toList).toList match {
        case List(k) => k
        case Nil => fail(f.getName +
          ": a .scup spec needs a companion .slex (its %kinds types the actions)")
        case ks => fail(f.getName + ": ambiguous %kinds (" + ks.mkString(", ") + ")")
      }
      val out = new File(outDir, m.objectName + ".scala")
      writeIfChanged(out, SpecAssemble.assembleScup(m, kinds))
      out
    }
    lexOut ++ scupOut
  } catch {
    case e: SpecAssemble.SpecError => fail(e.getMessage)
  }

  private def writeIfChanged(f: File, content: String): Unit = {
    val bytes = content.getBytes("UTF-8")
    val same = f.isFile && java.util.Arrays.equals(Files.readAllBytes(f.toPath), bytes)
    if (!same) {
      f.getParentFile.mkdirs()
      Files.write(f.toPath, bytes)
    }
  }
}
