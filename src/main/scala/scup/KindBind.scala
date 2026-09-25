package scup

import scala.reflect.ClassTag

/**
 * Binding a spec file's token-kind names to the host's hand-written
 * Scala 3 enum.
 *
 * Primary: the load site names the type --
 * `LexSpec.load[TokenKind]("ic.slex")` -- so a typo in the enum's
 * name is a compile error; the name -> value map comes from the
 * enum's `values()` forwarder on the statically-known class.
 *
 * Fallback (host-less tools): `%kinds pkg.TokenKind` plus
 * Class.forName; both paths converge on the same map.
 */
object KindBind:

  def kindMap[K](using ct: ClassTag[K]): Map[String, K] =
    fromClass(ct.runtimeClass).asInstanceOf[Map[String, K]]

  /** Reflective fallback: `fqcn` must name a Scala 3 enum. */
  def kindMapReflect(fqcn: String): Map[String, Any] =
    val cls =
      try Class.forName(fqcn)
      catch case _: ClassNotFoundException =>
        throw SpecAssemble.diagnostic(new RuntimeException(
          "%kinds names " + fqcn + ", which is not on the classpath"))
    fromClass(cls)

  private def fromClass(cls: Class[?]): Map[String, Any] =
    val values =
      try cls.getMethod("values").invoke(null).asInstanceOf[Array[AnyRef]]
      catch case _: NoSuchMethodException =>
        throw SpecAssemble.diagnostic(new RuntimeException(
          "token kinds must be a Scala 3 enum; " + cls.getName + " has no values()"))
    // a ListMap keeps the enum's declaration order, so anything that
    // lists kinds (like slex.check) reads the way the enum does
    scala.collection.immutable.ListMap.from(
      values.map(v => v.toString -> (v: Any)))
