package scup

/**
 * Where spec files come from at run time: the filesystem when the
 * program runs in its project directory (the normal sbt case), else
 * the classpath -- the build bundles project-root specs into the jar
 * as resources, so an assembled `icc.jar` (or a grader running it
 * from a scratch directory) is self-contained.
 */
object SpecFile:

  def readText(path: String): String =
    val p = java.nio.file.Path.of(path)
    if java.nio.file.Files.isReadable(p) then
      java.nio.file.Files.readString(p)
    else
      val name = p.getFileName.toString
      val in = Option(Thread.currentThread.getContextClassLoader)
        .getOrElse(getClass.getClassLoader)
        .getResourceAsStream(name)
      if in == null then
        throw SpecAssemble.diagnostic(new java.io.FileNotFoundException(
          s"spec file not found: $path (not on disk, and no '$name' " +
          "resource on the classpath)"))
      try new String(in.readAllBytes(), "UTF-8")
      finally in.close()
