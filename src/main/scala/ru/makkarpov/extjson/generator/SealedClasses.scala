package ru.makkarpov.extjson.xgenerator

import ru.makkarpov.extjson.annotations._

trait SealedClasses { this: Macros =>
  import c.universe._

  class SealedClassPlugin extends GenerationPlugin {
    override def appliesTo(tpe: Type): Boolean =
      tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed

    private def getSubclasses(tpe: Type, ctx: GenerationContext): Seq[Symbol] = {
      val clazz = tpe.typeSymbol.asClass
      var subclasses = clazz.knownDirectSubclasses.toVector

      if (subclasses.isEmpty) {
        // https://issues.scala-lang.org/browse/SI-7046
        clazz.typeSignature
        subclasses = clazz.knownDirectSubclasses.toVector
      }

      if (annotationPresent[addSubclass[_]](clazz)) {
        val added = clazz.annotations
          .filter(_.tree.tpe.dealias.erasure =:= typeOf[addSubclass[_]].erasure)
          .map(_.tree.tpe.typeArgs.head)

        added.filterNot(_ <:< tpe) match {
          case Nil =>
          case x => ctx.abort(s"Found @addSubclass with non-subclass arguments: ${x.mkString(", ")}")
        }

        subclasses ++= added.map(_.typeSymbol)
      }

      subclasses = subclasses.distinct

      if (subclasses.isEmpty)
        ctx.abort(
          s"""No subclasses found for this class. If there clearly are child classes,
             |try moving this call/referenced class into a separate file, package or project,
             |or use @addSubclass annotation to specify them manually.
             |
             |https://issues.scala-lang.org/browse/SI-7046
             """.stripMargin)

      subclasses
    }

    override def getDependencies(tpe: Type, ctx: GenerationContext): Seq[GenerationDependency] =
      getSubclasses(tpe, ctx).map(dependency)

    override def generate(tpe: Type, ctx: GenerationContext): Tree = {
      val clazz = tpe.typeSymbol.asClass
      val subclasses = getSubclasses(tpe, ctx)
      val dependencies = getDependencies(tpe, ctx)
      val serializers = subclasses.zipWithIndex.map {
        case (sc, i) => sc -> ctx.serializer(dependencies(i))
      }.toMap

      val fallbackClass = {
        val found = subclasses.filter(annotationPresent[fallbackCase])

        if (found.size > 1)
          ctx.abort("Multiple @fallbackCase's specified")

        found.headOption
      }

      def taggedSerializer: Tree = {
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

        def generateWrites: Tree = {
          val inputParameter = TermName("obj")

          val matches = subclasses.zipWithIndex.map { case (sc, i) =>
            val pat = TermName("x")
            cq"$pat: $sc => $ownPkg.JsonUtils.addType($tField, ${tValues(i)}, ${serializers(sc)}.writes($pat))"
          }

          q"""
            def writes($inputParameter: $tpe): $jsonPkg.JsValue = $inputParameter match { case ..$matches }
          """
        }

        def generateReads: Tree = {
          val inputParameter = TermName("js")
          val typeVar = TermName("type")
          val dataVar = TermName("data")
          val retVar = TermName("ret")

          var matches = subclasses.zipWithIndex.map { case (sc, i) =>
            cq"${tValues(i)} => ${serializers(sc)}.reads($dataVar)"
          }

          fallbackClass match {
            case Some(fc) =>
              matches :+= cq"_ => ${serializers(fc)}.reads($dataVar)"

            case None =>
              val pat = TermName("x")
              val msg = "unknown type"
              matches :+= cq"$pat => $jsonPkg.JsError($msg + $pat)"
          }

          q"""
            def reads($inputParameter: $jsonPkg.JsValue): $jsonPkg.JsResult[$tpe] =
              for {
                ($dataVar, $typeVar) <- $ownPkg.JsonUtils.stripType($tField, $inputParameter)
                $retVar <- $typeVar match { case ..$matches }
              } yield $retVar
          """
        }

        q"""
          new $jsonPkg.Format[$tpe] {
            $generateWrites
            $generateReads
          }
        """
      }

      def untaggedSerializer: Tree = {
        val sortedClasses = subclasses.sortWith((a, b) =>
          if (fallbackClass.contains(a)) false
          else if (fallbackClass.contains(b)) true
          else false
        )

        def generateReads: Tree = {
          val inputParameter = TermName("js")
          val errs = sortedClasses.map { sc =>
            val name = TermName(c.freshName("error"))
            val pat = TermName("x")
            name -> q"""
              val $name = ${serializers(sc)}.reads($inputParameter) match {
                case $jsonPkg.JsSuccess($pat, _) => return $jsonPkg.JsSuccess($pat)
                case $pat: $jsonPkg.JsError => $pat
              }
            """
          }

          q"""
            def reads($inputParameter: $jsonPkg.JsValue): $jsonPkg.JsResult[$tpe] = {
              ..${errs.map(_._2)}
              $ownPkg.JsonUtils.mergeErrors(..${errs.map(_._1)})
            }
          """
        }

        def generateWrites: Tree = {
          val inputParameter = TermName("obj")
          val matches = subclasses.zipWithIndex.map { case (sc, i) =>
            val pat = TermName("x")
            cq"$pat: $sc => ${serializers(sc)}.writes($pat)"
          }

          q"def writes($inputParameter: $tpe): $jsonPkg.JsValue = $inputParameter match { case ..$matches }"
        }

        q"""
          new $jsonPkg.Format[$tpe] {
            $generateReads
            $generateWrites
          }
        """
      }

      if (annotationPresent[formatInline](clazz)) untaggedSerializer
      else taggedSerializer
    }
  }
}
