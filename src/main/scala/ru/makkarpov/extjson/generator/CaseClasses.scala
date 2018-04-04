package ru.makkarpov.extjson.xgenerator

import ru.makkarpov.extjson.annotations._

trait CaseClasses { this: Macros =>
  import c.universe._

  private def toSnakeCase(s: String): String = s.replaceAll("([a-z])([A-Z])", "$1_$2").toLowerCase()

  class CaseClassPlugin extends GenerationPlugin {
    override def appliesTo(tpe: Type): Boolean =
      tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass

    override def getDependencies(tpe: Type, ctx: GenerationContext): Seq[GenerationDependency] = {
      val clazz = tpe.typeSymbol.asClass
      val args = clazz.primaryConstructor.asMethod.paramLists.flatten.toVector

      if (args.isEmpty)
        ctx.abort("Cannot create formatter for a case class with no arguments")

      val globalPlainOption = annotationPresent[plainOption](clazz)

      args.map { a =>
        val dep =
          if (globalPlainOption || annotationPresent[plainOption](a) || !(toType(a) <:< typeOf[Option[_]])) dependency(a)
          else dependency(toType(a).typeArgs.head)

        dep.withSettings(_.copy(fromString = annotationPresent[asString](a)))
      }
    }

    override def generate(tpe: Type, ctx: GenerationContext): c.universe.Tree = {
      val clazz = tpe.typeSymbol.asClass
      val objType = clazz.toType.companion
      val args = clazz.primaryConstructor.asMethod.paramLists.flatten.toVector
      val deps = getDependencies(tpe, ctx)

      val obj = clazz.companionSymbol
      val fieldNames = args.map(_.name.decodedName.toString)

      if (obj == NoSymbol)
        ctx.abort(
          s"""Internal error: failed to find companion object for ${clazz.fullName}.
             |
             |If it is an inner class, consider moving it to outer levels.
             """.stripMargin)

      if (annotationPresent[formatInline](clazz)) {
        if (deps.length > 1)
          ctx.abort("Cannot @formatInline case class with multiple arguments")

        val readsParameter = TermName("js")
        val writesParameter = TermName("obj")
        val serializerName = ctx.serializer(deps.head)
        val fieldName = TermName(fieldNames.head)

        return q"""
          new $jsonPkg.Format[$tpe] {
            def reads($readsParameter: $jsonPkg.JsValue): $jsonPkg.JsResult[$tpe] =
              $serializerName.reads($readsParameter).map($obj(_))

            def writes($writesParameter: $tpe): $jsonPkg.JsValue =
              $serializerName.writes($writesParameter.$fieldName)
          }
        """
      }

      val globalSnakeCase = annotationPresent[snakeCase](clazz)

      val jsonNames = args.map { sym =>
        if (annotationPresent[key](sym)) stringLiteral(singleArgAnnotation[key](sym)) match {
          case Some(s) => s
          case None => ctx.abort("value of @key annotation must be a string literal")
        } else if (globalSnakeCase || annotationPresent[snakeCase](sym)) toSnakeCase(sym.name.decodedName.toString)
        else sym.name.decodedName.toString
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

      val serializers: Seq[Tree] = deps.map(ctx.serializer)

      def generateReads: Tree = {
        val inputParameter = TermName("js")

        val termNames = fieldNames.map(TermName(_))
        val defs = termNames.zipWithIndex.map { case (tn, i) =>
          val name = jsonNames(i)

          if (optionalFields(i)) q"val $tn = ($inputParameter \ $name).validateOpt(${serializers(i)})"
          else {
            var reads = q"($inputParameter \ $name).validate(${serializers(i)})"
            for ((d, _) <- defaults(i))
              reads = q"$reads.orElse($jsonPkg.JsSuccess($d))"

            q"val $tn = $reads"
          }
        }

        q"""
        def reads($inputParameter: _root_.play.api.libs.json.JsValue): $jsonPkg.JsResult[$tpe] = {
          ..$defs
          $ownPkg.JsonUtils.mergeResults[$tpe](..$termNames)(new $clazz(..${termNames.map(tn => q"$tn.get")}))
        }
      """
      }

      def generateWrites: Tree = {
        val inputParameter = TermName("obj")
        val builder = TermName("bld")

        val appends = jsonNames.zipWithIndex.map { case (n, i) =>
          val tn = TermName(fieldNames(i))

          def write(t: Tree): Tree = q"$builder += (($n, ${serializers(i)}.writes($t)))"

          if (optionalFields(i)) q"if ($inputParameter.$tn.isDefined) ${write(q"$inputParameter.$tn.get")}"
          else defaults(i) match {
            case None | Some((_, true)) => write(q"$inputParameter.$tn")
            case Some((d, false)) => q"if ($d != $inputParameter.$tn) ${write(q"$inputParameter.$tn")}"
          }
        }

        q"""
          def writes($inputParameter: $tpe): $jsonPkg.JsValue = {
            val $builder = _root_.scala.collection.Map.newBuilder[_root_.scala.Predef.String, $jsonPkg.JsValue]
            ..$appends
            $jsonPkg.JsObject($builder.result())
          }
        """
      }

      q"""
        new $jsonPkg.Format[$tpe] {
          ..$generateReads
          ..$generateWrites
        }
       """
    }
  }
}
