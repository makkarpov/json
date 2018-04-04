package ru.makkarpov.extjson.xgenerator

import ru.makkarpov.extjson.StrFormat

trait Collections { this: Macros =>
  import c.universe._

  private def check[T](tpe: Type)(implicit tt: TypeTag[T]): Boolean =
    tpe.erasure <:< tt.tpe.erasure

  private val canBuildFrom = tq"_root_.scala.collection.generic.CanBuildFrom"

  class TraversablePlugin extends GenerationPlugin {
    override def appliesTo(tpe: Type): Boolean = check[Traversable[_]](tpe) && tpe.typeArgs.size == 1

    override def getDependencies(tpe: Type, ctx: GenerationContext): Seq[GenerationDependency] = {
      val TypeRef(_, _, sub :: Nil) = tpe
      dependency(sub) :: Nil
    }

    override def generate(tpe: Type, ctx: GenerationContext): Tree = {
      val serializer = ctx.serializer(getDependencies(tpe, ctx).head)
      val tsym = tpe.typeSymbol
      val TypeRef(_, _, sub :: Nil) = tpe

      val cbf = resolveImplicit(Nil, tq"$canBuildFrom[$tsym[_], $sub, $tsym[$sub]]")

      if (cbf.isEmpty)
        ctx.abort(s"Failed to find CanBuildFrom for ${show(q"$tsym[$sub]")}")

      q"$ownPkg.JsonUtils.traversableFormat[$tsym, $sub]($cbf, $serializer)"
    }
  }

  class MapPlugin extends GenerationPlugin {
    override def appliesTo(tpe: Type): Boolean = check[Map[_, _]](tpe) && tpe.typeArgs.size == 2

    override def getDependencies(tpe: Type, ctx: GenerationContext): Seq[GenerationDependency] = {
      val TypeRef(_, _, _ :: tval :: Nil) = tpe
      dependency(tval) :: Nil
    }

    override def generate(tpe: Type, ctx: GenerationContext): Tree = {
      val TypeRef(_, _, tkey :: tval :: Nil) = tpe
      val tsym = tpe.typeSymbol
      val kfmt = implicitApplied(tkey)(typeOf[StrFormat[_]], Nil)

      if (kfmt.isEmpty)
        ctx.abort(
          s"""Failed to find a key formatter for $tkey.
             |Consider implementing an implicit StrFormat[$tkey]
             """.stripMargin)

      val cbf = resolveImplicit(Nil, tq"$canBuildFrom[$tsym[_, _], ($tkey, $tval), $tsym[$tkey, $tval]]")

      if (cbf.isEmpty)
        ctx.abort(s"Failed to find CanBuildFrom for ${show(q"$tsym[$tkey, $tval]")}")

      val vfmt = ctx.serializer(getDependencies(tpe, ctx).head)
      q"$ownPkg.JsonUtils.mapFormat[$tsym, $tkey, $tval]($kfmt, $vfmt, $cbf)"
    }
  }

  class EnumerationPlugin extends GenerationPlugin {
    override def appliesTo(tpe: Type): Boolean = tpe match {
      case TypeRef(base, _, Nil) => check[Enumeration](base)
      case _ => false
    }

    override def getDependencies(tpe: Type, ctx: GenerationContext): Seq[GenerationDependency] = Nil

    override def generate(tpe: Type, ctx: GenerationContext): Tree = {
      val TypeRef(base, value, Nil) = tpe

      val obj = base.typeSymbol.companionSymbol match { // `companionSymbol` is necessary here
        case NoSymbol => ctx.abort(s"Failed to find companion for ${show(base)}")
        case t if t.isTerm => t
        case _ => ctx.abort(s"Companon symbol for ${show(base)} is not a term")
      }

      value.name.decodedName.toString match {
        case "Value" => q"$ownPkg.JsonUtils.enumerationFormat($obj)"
        case "ValueSet" => q"$ownPkg.JsonUtils.enumerationSetFormat($obj)"
        case x => ctx.abort(s"Unknown Enumeration inner class: $x")
      }
    }
  }

  class OptionPlugin extends GenerationPlugin {
    override def appliesTo(tpe: c.universe.Type): Boolean = check[Option[_]](tpe) && tpe.typeArgs.size == 1

    override def getDependencies(tpe: c.universe.Type, ctx: GenerationContext): Seq[GenerationDependency] = {
      val TypeRef(_, _, arg :: Nil) = tpe
      dependency(arg) :: Nil
    }

    override def generate(tpe: c.universe.Type, ctx: GenerationContext): c.universe.Tree = {
      val TypeRef(_, _, arg :: Nil) = tpe
      q"$ownPkg.JsonUtils.optionFormat[$arg](${ctx.serializer(getDependencies(tpe, ctx).head)})"
    }
  }

  class TuplePlugin extends GenerationPlugin {
    override def appliesTo(tpe: Type): Boolean =
      tpe.typeSymbol.isClass && isTuple(tpe.typeSymbol.asClass)

    override def getDependencies(tpe: Type, ctx: GenerationContext): Seq[GenerationDependency] =
      tpe.typeArgs.map(dependency)

    override def generate(tpe: Type, ctx: GenerationContext): Tree = {
      val subs = tpe.typeArgs
      val funcName = TermName(s"tuple${subs.size}Format")
      val deps = getDependencies(tpe, ctx).map(ctx.serializer)

      q"$ownPkg.JsonFormats.$funcName[..$subs](..$deps)"
    }
  }
}
