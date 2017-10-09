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

import ru.makkarpov.extjson.KeyFormat

trait Miscellaneous { this: Macros =>
  import c.universe._

  val canBuildFrom = tq"_root_.scala.collection.generic.CanBuildFrom"

  def generateCollection(ctx: GenerationContext, tpe: Type): Tree = {
    def check[T](implicit tt: TypeTag[T]): Boolean = tpe.erasure <:< tt.tpe.erasure
    val tsym = tpe.typeSymbol

    tpe match {
      case TypeRef(_, _, sub :: Nil) if check[Traversable[_]] =>
        val cbf = resolveImplicit(Nil, tq"$canBuildFrom[$tsym[_], $sub, $tsym[$sub]]")

        if (cbf.isEmpty)
          ctx.abort(s"Failed to find CanBuildFrom for ${show(q"$tsym[$sub]")}")

        val format = ctx.subGenerate(sub)

        q"$ownPkg.JsonUtils.traversableFormat[$tsym, $sub]($cbf, $format)"

      case TypeRef(_, _, tkey :: tval :: Nil) if check[Map[_, _]] =>
        val kfmt = implicitApplied(tkey)(typeOf[KeyFormat[_]], Nil)

        if (kfmt.isEmpty)
          ctx.abort(
            s"""Failed to find a key formatter for $tkey.
               |Consider implementing an implicit KeyFormat[$tkey]
             """.stripMargin)

        val cbf = resolveImplicit(Nil, tq"$canBuildFrom[$tsym[_, _], ($tkey, $tval), $tsym[$tkey, $tval]]")

        if (cbf.isEmpty)
          ctx.abort(s"Failed to find CanBuildFrom for ${show(q"$tsym[$tkey, $tval]")}")

        val vfmt = ctx.subGenerate(tval)

        q"$ownPkg.JsonUtils.mapFormat[$tsym, $tkey, $tval]($kfmt, $vfmt, $cbf)"

      case TypeRef(_, _, sub :: Nil) if check[Option[_]] =>
        q"$ownPkg.JsonUtils.optionFormat[$sub](${ctx.subGenerate(sub)})"

      case _ => EmptyTree
    }
  }

  def generateMisc(ctx: GenerationContext, tpe: Type): Tree = tpe match {
    case TypeRef(base, value, Nil) if base <:< weakTypeOf[Enumeration] =>
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
    case _ => EmptyTree
  }
}
