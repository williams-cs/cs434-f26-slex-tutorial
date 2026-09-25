package scup

import java.io.File
import java.nio.file.Files

/**
 * Per-project VSCode wiring, run from build.sbt's `Global / onLoad`
 * hook: simply opening the project in sbt/Metals recommends the
 * "slex & scup" extension (and Metals) via .vscode/extensions.json.
 *
 * Writes are content-idempotent (nothing is touched when the file
 * already says what we need -- so mtime-based tooling never thrashes)
 * and merge-not-clobber (only the recommendations this task owns are
 * added; user additions survive; a file we cannot understand is left
 * alone).
 *
 * Compiled by the sbt metabuild (Scala 2.12); synced from
 * idea/scup/project/ like SpecGen.scala.
 */
object VscodeSetup {

  private val owned = List("cs434.slex-scup", "scalameta.metals")

  def apply(baseDir: File): Unit = {
    try setup(baseDir)
    catch { case _: Exception => () } // editor setup must never break a build
  }

  private def setup(baseDir: File): Unit = {
    val dir = new File(baseDir, ".vscode")
    val f = new File(dir, "extensions.json")
    if (!f.isFile) {
      dir.mkdirs()
      write(f, canonical(owned))
      return
    }
    val text = new String(Files.readAllBytes(f.toPath), "UTF-8")
    val missing = owned.filterNot(id => text.contains("\"" + id + "\""))
    if (missing.isEmpty) return
    // insert into an existing "recommendations": [ ... ] array; if the
    // file's shape is unexpected, leave it alone (never clobber)
    val marker = "\"recommendations\""
    val at = text.indexOf(marker)
    if (at < 0) return
    val open = text.indexOf('[', at)
    if (open < 0) return
    val inserted = missing.map(id => "\"" + id + "\"").mkString(", ") +
      (if (text.substring(open + 1).trim.startsWith("]")) "" else ", ")
    write(f, text.substring(0, open + 1) + inserted + text.substring(open + 1))
  }

  private def canonical(ids: List[String]): String =
    ids.map(id => "    \"" + id + "\"").mkString(
      "{\n  \"recommendations\": [\n", ",\n", "\n  ]\n}\n")

  private def write(f: File, content: String): Unit = {
    val bytes = content.getBytes("UTF-8")
    val same = f.isFile && java.util.Arrays.equals(Files.readAllBytes(f.toPath), bytes)
    if (!same) Files.write(f.toPath, bytes)
  }
}
