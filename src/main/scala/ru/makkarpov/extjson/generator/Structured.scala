/******************************************************************************
 * Copyright © 2017 Maxim Karpov                                              *
 *                                                                            *
 * Licensed under the Apache License, Version 2.0 (the "License");            *
 * you may not use this file except in compliance with the License.           *
 * You may obtain a copy of the License at                                    *
 *                                                                            *
 *     http://www.apache.org/licenses/LICENSE-2.0                             *
 *                                                                            *
 * Unless required by applicable law or agreed to in writing, software        *
 * distributed under the License is distributed on an "AS IS" BASIS,          *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.   *
 * See the License for the specific language governing permissions and        *
 * limitations under the License.                                             *
 ******************************************************************************/

package ru.makkarpov.extjson.generator

import ru.makkarpov.extjson.annotations._

trait Structured { this: Macros =>
  import c.universe._

  def generateCaseClass(ctx: GenerationContext, clazz: ClassSymbol): Tree = {
    val obj = clazz.companion

    if (obj == NoSymbol)
      ctx.abort(
        s"""Failed to find companion object for ${clazz.fullName}.
           |
           |If it is inner class, consider moving it to outer levels.
         """.stripMargin)

    val clazzType = clazz.toType
    val objType = clazzType.companion
    val args = clazz.primaryConstructor.asMethod.paramLists.flatten.toVector

    if (args.isEmpty)
      ctx.abort("Cannot create formatter for a case class with no arguments")

    val fieldNames = args.map(_.name.decodedName.toString)

    if (annotationPresent[formatInline](clazz)) {
      if (args.length > 1)
        ctx.abort("Cannot @formatInline case class with multiple arguments")

      val serializerName = TermName("serializer")
      val serializer = ctx.subGenerate(args.head)
      val readsParameter = TermName("js")
      val writesParameter = TermName("obj")
      val fieldName = TermName(fieldNames.head)

      return q"""
        new $jsonPkg.Format[$clazzType] {
          val $serializerName = $serializer

          def reads($readsParameter: $jsonPkg.JsValue): $jsonPkg.JsResult[$clazzType] =
            $serializerName.reads($readsParameter).map($obj(_))

          def writes($writesParameter: $clazzType): $jsonPkg.JsValue =
            $serializerName.writes($writesParameter.$fieldName)
        }
      """
    }

    val jsonNames = args.map { sym =>
      if (annotationPresent[key](sym)) stringLiteral(singleArgAnnotation[key](sym)) match {
        case Some(s) => s
        case None => ctx.abort("value of @key annotation must be a string literal")
      } else sym.name.decodedName.toString
    }

    val defaults = {
      val globalSerializeDefaults = annotationPresent[serializeDefaults](clazz)

      args.zipWithIndex.map {
        case (sym, i) if sym.asTerm.isParamWithDefault =>
          val methodName = TermName(s"apply$$default$$${i + 1}")
          objType.member(methodName) match {
            case NoSymbol => ctx.abort(s"Failed to find default value for parameter ${sym.name.decodedName}.")
            case _ =>
              val serializeDefault = annotationPresent[serializeDefaults](sym)
              Some(q"$obj.$methodName" -> (globalSerializeDefaults || serializeDefault))
          }

        case _ => None
      }
    }

    val optionalFields =
      if (annotationPresent[plainOption](clazz)) args.map(_ => false)
      else args.map(sym => !sym.asTerm.isParamWithDefault && (toType(sym) <:< typeOf[Option[_]])
                        && !annotationPresent[plainOption](sym))

    val serializerNames = args.map(_ => TermName(c.freshName("serializer")))

    val serializerDefs = args.zipWithIndex.map { case (a, i) =>
      val ser = ctx.subGenerate(if (optionalFields(i)) toType(a).typeArgs.head else toType(a))
      q"val ${serializerNames(i)} = $ser"
    }

    def generateReads: Tree = {
      val inputParameter = TermName("js")

      val termNames = fieldNames.map(TermName(_))
      val defs = termNames.zipWithIndex.map { case (tn, i) =>
        val name = jsonNames(i)

        if (optionalFields(i)) q"val $tn = ($inputParameter \ $name).validateOpt(${serializerNames(i)})"
        else {
          var reads = q"($inputParameter \ $name).validate(${serializerNames(i)})"
          for ((d, _) <- defaults(i))
            reads = q"$reads.orElse($jsonPkg.JsSuccess($d))"

          q"val $tn = $reads"
        }
      }

      q"""
        def reads($inputParameter: _root_.play.api.libs.json.JsValue): $jsonPkg.JsResult[$clazzType] = {
          ..$defs
          $ownPkg.JsonUtils.mergeErrors[$clazzType](..$termNames)(new $clazz(..${termNames.map(tn => q"$tn.get")}))
        }
      """
    }

    def generateWrites: Tree = {
      val inputParameter = TermName("obj")
      val builder = TermName("bld")

      val appends = jsonNames.zipWithIndex.map { case (n, i) =>
        val tn = TermName(fieldNames(i))

        def write(t: Tree): Tree = q"$builder += (($n, ${serializerNames(i)}.writes($t)))"

        if (optionalFields(i)) q"if ($inputParameter.$tn.isDefined) ${write(q"$inputParameter.$tn.get")}"
        else defaults(i) match {
          case None | Some((_, true)) => write(q"$inputParameter.$tn")
          case Some((d, false)) => q"if ($d != $inputParameter.$tn) ${write(q"$inputParameter.$tn")}"
        }
      }

      q"""
        def writes($inputParameter: $clazzType): $jsonPkg.JsValue = {
          val $builder = _root_.scala.collection.Map.newBuilder[_root_.scala.Predef.String, $jsonPkg.JsValue]
          ..$appends
          $jsonPkg.JsObject($builder.result())
        }
      """
    }

    val ret =
      q"""
        new $jsonPkg.Format[$clazzType] {
          ..$serializerDefs

          $generateReads
          $generateWrites
        }
      """

    ret
  }

  def generateSealedClass(ctx: GenerationContext, clazz: ClassSymbol): Tree = {
    val clazzType = clazz.toType
    var subclasses = clazz.knownDirectSubclasses.toVector

    if (subclasses.isEmpty) {
      // https://issues.scala-lang.org/browse/SI-7046
      clazz.typeSignature
      subclasses = clazz.knownDirectSubclasses.toVector
    }

    if (annotationPresent[addSubclass[_]](clazz))
      subclasses ++= clazz.annotations
        .filter(_.tree.tpe.dealias.erasure =:= typeOf[addSubclass[_]].erasure)
        .map(_.tree.tpe.typeArgs.head.typeSymbol)

    subclasses = subclasses.distinct

    if (subclasses.isEmpty)
      ctx.abort(
        s"""No subclasses found for this class. If there clearly are child classes,
           |try moving this call/referenced class into a separate file, package or project,
           |or use @addSubclass annotation to specify them manually.
           |
           |https://issues.scala-lang.org/browse/SI-7046
         """.stripMargin)

    val fallbackClass = {
      val found = subclasses.filter(annotationPresent[fallbackCase])

      if (found.size > 1)
        ctx.abort("Multiple @fallbackCase's specified")

      found.headOption
    }

    val tField =
      if (annotationPresent[typeField](clazz)) stringLiteral(singleArgAnnotation[typeField](clazz)) match {
        case Some(s) => s
        case None => ctx.abort("Only string literals are allowed for @typeField(...)")
      } else "type"

    val tValues = {
      val r = subclasses.map { sc =>
        if (annotationPresent[typeValue](sc)) stringLiteral(singleArgAnnotation[typeValue](sc)) match {
          case Some(s) => s
          case None => ctx.abort("Only string literals are allowed for @typeValue(...)")
        } else sc.name.decodedName.toString
      }

      val dups = r.diff(r.distinct).distinct

      if (dups.nonEmpty)
        ctx.abort(s"Duplicate types: ${dups.mkString(", ")}")

      r
    }

    val serializerNames = subclasses.map(_ => TermName(c.freshName("serializer")))

    val serializerDefs = subclasses.zipWithIndex.map { case (a, i) =>
      q"val ${serializerNames(i)} = ${ctx.subGenerate(a)}"
    }

    def generateWrites: Tree = {
      val inputParameter = TermName("obj")

      val matches = subclasses.zipWithIndex.map { case (sc, i) =>
        val pat = TermName("x")

        cq"$pat: $sc => $ownPkg.JsonUtils.addType($tField, ${tValues(i)}, ${serializerNames(i)}.writes($pat))"
      }

      q"""
        def writes($inputParameter: $clazzType): $jsonPkg.JsValue = $inputParameter match { case ..$matches }
      """
    }

    def generateReads: Tree = {
      val inputParameter = TermName("js")
      val typeVar = TermName("type")
      val dataVar = TermName("data")
      val retVar = TermName("ret")

      var matches = subclasses.indices.map(i => cq"${tValues(i)} => ${serializerNames(i)}.reads($dataVar)")

      fallbackClass match {
        case Some(fc) =>
          val i = subclasses.indexWhere(_ == fc)

          if (i == -1)
            ctx.abort("Internal error: failed to find fallbackClass in subclasses")

          matches :+= cq"_ => ${serializerNames(i)}.reads($dataVar)"

        case None =>
          val pat = TermName("x")
          val msg = "unknown type"
          matches :+= cq"$pat => $jsonPkg.JsError($msg + $pat)"
      }

      q"""
        def reads($inputParameter: $jsonPkg.JsValue): $jsonPkg.JsResult[$clazzType] =
          for {
            ($dataVar, $typeVar) <- $ownPkg.JsonUtils.stripType($tField, $inputParameter)
            $retVar <- $typeVar match { case ..$matches }
          } yield $retVar
      """
    }

    val ret = q"""
      new $jsonPkg.Format[$clazzType] {
        ..$serializerDefs

        $generateWrites
        $generateReads
      }
    """

    ret
  }

  def generateTuple(ctx: GenerationContext, clazz: ClassSymbol): Tree = {
    ???
  }
}
