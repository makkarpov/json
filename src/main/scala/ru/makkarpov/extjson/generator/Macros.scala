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

import play.api.libs.json.Format
import ru.makkarpov.extjson.annotations.requireImplicit

import scala.reflect.macros.whitebox

object Macros {
  // Calling macro bundle directly produced some weird errors like
  // > IllegalArgumentException: wrong number of arguments
  // before the first line of macro bundle was executed.
  def generateImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    val tag = implicitly[c.WeakTypeTag[T]]
    val mm = new Macros(c)
    mm.generate(tag.asInstanceOf[mm.c.WeakTypeTag[T]]).asInstanceOf[c.Tree]
  }

  def materializeFormat[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    val underlying = generateImpl(c)
    q"_root_.ru.makkarpov.extjson.GeneratedFormat($underlying)"
  }
}

class Macros(val c: whitebox.Context) extends Utils with Structured with Miscellaneous {
  import c.universe._

  val jsonPkg = q"_root_.play.api.libs.json"
  val ownPkg = q"_root_.ru.makkarpov.extjson"

  object GenerationContext {
    def requireImplicit(x: Symbol): Boolean = annotationPresent[requireImplicit](x)
  }

  case class GenerationContext(initial: Boolean, requireImplicit: Boolean, generationStack: List[Type]) {
    def fallback(t: Type): GenerationContext = copy(
      requireImplicit = requireImplicit || GenerationContext.requireImplicit(t.typeSymbol)
    )

    def subGenerate(sym: Symbol): Tree = generate(toType(sym), copy(
      initial = false,
      requireImplicit = GenerationContext.requireImplicit(sym),
      generationStack = toType(sym) :: generationStack
    ))

    def subGenerate(t: Type): Tree = generate(t, copy(
      initial = false,
      requireImplicit = GenerationContext.requireImplicit(t.typeSymbol),
      generationStack = t :: generationStack
    ))

    def abort(msg: String): Nothing = {
      val currentType = show(generationStack.head)
      val outerTypes = generationStack.tail.map(show(_)).reverse.mkString(" -> ")
      val stackStr = if (outerTypes.isEmpty) "" else s"\nCalled from: $outerTypes"

      c.abort(c.enclosingPosition,
        s"""In generation of Format[$currentType]:$stackStr
           |
           |${msg.trim}
         """.stripMargin)
    }
  }

  def generate(bt: Type, bctx: GenerationContext): Tree = {
    val t = bt.dealias
    val ctx = bctx.fallback(t)

    var ret = implicitApplied(t)(typeOf[Format[_]], Seq(q"import $ownPkg.JsonFormats._"))
    if (ret.nonEmpty)
      return ret

    if (ctx.requireImplicit && !ctx.initial)
      ctx.abort("@requireImplicit annotation is present, refusing to generate")

    if (t.typeSymbol.isClass) {
      val clazz = t.typeSymbol.asClass

      if (clazz.isSealed) {
        ret = generateSealedClass(ctx, clazz)
      } else if (isTuple(clazz)) {
        ret = generateTuple(ctx, t)
      } else if (clazz.isCaseClass) {
        ret = generateCaseClass(ctx, clazz)
      }
    }

    if (ret.isEmpty)
      ret = generateCollection(ctx, t)

    if (ret.isEmpty)
      ret = generateMisc(ctx, t)

    if (ret.isEmpty)
      ctx.abort("Unknown type: could not generate formatter and no implicit Format found.")

    ret
  }

  def generate[T](tag: WeakTypeTag[T]): Tree =
    generate(tag.tpe, GenerationContext(initial = true, requireImplicit = false, tag.tpe :: Nil))
}
