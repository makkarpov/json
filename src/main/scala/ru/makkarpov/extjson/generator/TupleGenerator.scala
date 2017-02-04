package ru.makkarpov.extjson.generator

object TupleGenerator extends App {
  val arityRange = 2 to 8

  println("// Generated file")
  println("package ru.makkarpov.extjson")
  println("")
  println("import play.api.libs.json._")
  println("")
  println("object Tuples {")

  def tupleType(args: Seq[String]): String = s"(${args.mkString(",")})"

  for (n <- arityRange) {
    val typeArgs = (1 to n).map(i => s"T$i")
    val fmtArgs = (1 to n).map(i => s"fmt$i")
    val patArgs = (1 to n).map(i => s"p$i")
    val resArgs = (1 to n).map(i => s"r$i")
    val tuple = tupleType(typeArgs)

    val fmtStr = fmtArgs.zipWithIndex.map{ case (a, i) => s"$a:Format[${typeArgs(i)}]" }.mkString(",")
    val writesStr = fmtArgs.zipWithIndex.map{ case (f, i) => s"$f.writes(t._${i+1})" }.mkString(",")

    println(s"  class Tuple${n}Format[${typeArgs.mkString(",")}]($fmtStr)")
    println(s"  extends Format[$tuple] {")
    println(s"    def writes(t: $tuple): JsValue = JsArray(Seq($writesStr))")
    println(s"")
    println(s"    def reads(j: JsValue): JsResult[$tuple] = j match {")
    println(s"      case JsArray(Seq(${patArgs.mkString(", ")})) =>")

    for (i <- 0 until n)
      println(s"        val ${resArgs(i)} = ${fmtArgs(i)}.reads(${patArgs(i)})")

    println(s"        if (${resArgs.map(_ + ".isSuccess").mkString("&&")}) JsSuccess((${resArgs.map(_ + ".get").mkString(",")}))")
    println(s"        else JsonUtils.mergeErrors(${resArgs.mkString(",")})")
//    println(s"        JsonUtils.mergeResults(${resArgs.mkString(",")})((${resArgs.map(_ + ".get").mkString(",")}))")

    println( "      case _ => JsError(\"jsArray expected\")")
    println(s"    }")
    println(s"  }")
    println("")
  }

  println("}")
  println("")
  println("trait Tuples {")

  for (n <- arityRange) {
    val typeArgs = (1 to n).map(i => s"T$i")
    val fmtArgs = (1 to n).map(i => s"fmt$i")
    val tuple = tupleType(typeArgs)
    val fmtStr = fmtArgs.zipWithIndex.map{ case (a, i) => s"$a:Format[${typeArgs(i)}]" }.mkString(",")

    println(s"  implicit def tuple${n}Format[${typeArgs.mkString(",")}](implicit $fmtStr): Format[$tuple] =")
    println(s"    new Tuples.Tuple${n}Format[${typeArgs.mkString(",")}](${fmtArgs.mkString(",")})")
  }

  println("}")
}
