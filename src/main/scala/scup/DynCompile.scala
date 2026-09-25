package scup

import java.nio.file.{Files, Path}

/**
 * The two ways a spec file's action blocks become loadable code.
 *
 * Path A (build time, primary): an sbt source generator
 * (project/SpecGen.scala) already compiled the synthetic actions
 * object with the project; `findCompiled` locates it and checks its
 * embedded spec hash.
 *
 * Path B (load time, host-less tools): `compileAndLoad` compiles the
 * synthetic source in-process with the Scala 3 compiler, invoked
 * reflectively (so this library never depends on scala3-compiler at
 * compile time), caching compiled classes under
 * ~/.cache/scup/<hash(source, scala version)>/.
 */
object DynCompile:

  final case class Loaded(instance: AnyRef, cls: Class[?], compiledNow: Boolean)

  def defaultCacheRoot: Path =
    Path.of(System.getProperty("user.home"), ".cache", "scup")

  /** Path A: the actions object compiled by the sbt source generator,
   *  or None if the class is not on the classpath.  `expectHash` is
   *  the current spec file's hash; a mismatch means the spec changed
   *  after the last build. */
  def findCompiled(objectName: String, expectHash: String): Option[Loaded] =
    val clsName = SpecAssemble.genPackage + "." + objectName + "$"
    try
      val cls = Class.forName(clsName)
      val inst = cls.getField("MODULE$").get(null).asInstanceOf[AnyRef]
      val hash = cls.getMethod("specHash").invoke(inst).asInstanceOf[String]
      if hash != expectHash then
        throw SpecAssemble.diagnostic(new RuntimeException(
          "spec changed since its actions were compiled -- rebuild (sbt compile)"))
      Some(Loaded(inst, cls, compiledNow = false))
    catch case _: ClassNotFoundException => None

  /** Path B: compile (or reuse the cache) and load the actions object. */
  def compileAndLoad(objectName: String, source: String,
                     cacheRoot: Path = defaultCacheRoot): Loaded =
    val key = SpecAssemble.sha256(
      source + "|" + scala.util.Properties.versionNumberString)
    val dir = cacheRoot.resolve(key)
    val marker = dir.resolve("ok")
    var compiledNow = false
    if !Files.exists(marker) then
      Files.createDirectories(dir)
      val srcFile = dir.resolve(objectName + ".scala")
      Files.write(srcFile, source.getBytes("UTF-8"))
      val args = Array("-d", dir.toString, "-classpath", runtimeClasspath,
        srcFile.toString)
      val (ok, output) = runDotc(args)
      if !ok then
        throw new RuntimeException("spec action compile failed:\n" + output)
      Files.write(marker, Array[Byte]())
      compiledNow = true
    val loader = new java.net.URLClassLoader(
      Array(dir.toUri.toURL), getClass.getClassLoader)
    val cls = Class.forName(
      SpecAssemble.genPackage + "." + objectName + "$", true, loader)
    val inst = cls.getField("MODULE$").get(null).asInstanceOf[AnyRef]
    Loaded(inst, cls, compiledNow)

  /** The classpath of the running application: java.class.path plus
   *  whatever the classloader chain exposes (under an unforked sbt
   *  test run, java.class.path is only the sbt launcher; the real
   *  classpath lives in sbt's URLClassLoaders). */
  private def runtimeClasspath: String =
    val entries = scala.collection.mutable.LinkedHashSet[String]()
    var cl = getClass.getClassLoader
    while cl != null do
      cl match
        case u: java.net.URLClassLoader =>
          for url <- u.getURLs do
            try entries += java.nio.file.Paths.get(url.toURI).toString
            catch case _: Exception => ()
        case _ => ()
      cl = cl.getParent
    entries += System.getProperty("java.class.path")
    entries.mkString(java.io.File.pathSeparator)

  /** Run dotc reflectively; returns (no errors, captured diagnostics). */
  def runDotc(args: Array[String]): (Boolean, String) =
    val driverCls =
      try Class.forName("dotty.tools.dotc.Driver")
      catch case _: ClassNotFoundException =>
        throw SpecAssemble.diagnostic(new RuntimeException(
          "the Scala 3 compiler is not on the classpath; add scala3-compiler " +
          "to build.sbt (or use the sbt source generator)"))
    val driver = driverCls.getDeclaredConstructor().newInstance()
    val process = driverCls.getMethods.find(m =>
        m.getName == "process" && m.getParameterCount == 3 &&
        m.getParameterTypes()(0) == classOf[Array[String]] &&
        m.getParameterTypes()(1).getName.endsWith("Reporter"))
      .getOrElse(throw SpecAssemble.diagnostic(new RuntimeException(
        "no Driver.process(args, reporter, callback) found")))
    val buf = new java.io.ByteArrayOutputStream
    val ps = new java.io.PrintStream(buf, true, "UTF-8")
    val oldErr = System.err
    val oldOut = System.out
    val reporter =
      try
        System.setErr(ps)
        System.setOut(ps)
        Console.withErr(ps) { Console.withOut(ps) {
          process.invoke(driver, args, null, null) } }
      finally
        System.setErr(oldErr)
        System.setOut(oldOut)
    val hasErrors =
      reporter.getClass.getMethod("hasErrors").invoke(reporter)
        .asInstanceOf[Boolean]
    (!hasErrors, buf.toString("UTF-8"))
