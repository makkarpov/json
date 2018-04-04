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

package ru.makkarpov.extjson.xgenerator

trait Utils { this: Macros =>
  import c.universe._

  val jsonPkg = q"_root_.play.api.libs.json"
  val ownPkg = q"_root_.ru.makkarpov.extjson"

  def toType(x: Symbol): Type = if (x.isType) x.asType.toType else x.typeSignature

  def annotationPresent[T: TypeTag](t: Symbol): Boolean =
    t.annotations.exists(_.tree.tpe.erasure.dealias =:= typeOf[T].erasure)

  def singleArgAnnotation[T: TypeTag](t: Symbol): Tree =
    t.annotations.find(_.tree.tpe =:= typeOf[T]).flatMap(_.tree.children.lastOption).getOrElse(EmptyTree)

  def isTuple(x: ClassSymbol): Boolean = x.baseClasses.exists(_.fullName.startsWith("scala.Tuple"))

  def stringLiteral(t: Tree): Option[String] = t match {
    case q"${str: String}" => Some(str)
    case _ => None
  }

  def resolveImplicit(imports: Seq[Tree], target: Tree): Tree =
    // We possibly could extract actual value since `implicitly` after typecheck holds actual
    // inferred value, but since Trees are not hygienic, extracted value could have a different
    // meaning in outside context.
    c.typecheck(
      q"""
        {
          ..$imports
          _root_.scala.Predef.implicitly[$target]
        }
      """, silent = true
    )

  def implicitApplied(t: Type)(baseType: Type, predefs: Seq[Tree]): Tree = {
    // Perform implicit search in following locations:
    //  1. Specified predefs
    //  2. In a companion object of `t`
    //  3. If `t` itself is inside of object - in all outer objects

    val searchLocations = Seq.newBuilder[Tree]

    searchLocations ++= predefs

    var comp = t.typeSymbol.companion
    while (comp != NoSymbol) {
      var tree: Tree = Ident(termNames.ROOTPKG)

      for (x <- comp.fullName.split("\\."))
        tree = Select(tree, TermName(x))

      searchLocations += q"import $tree._"

      comp = comp.owner
      if (comp.isPackage || !comp.isClass)
        comp = NoSymbol
    }

    resolveImplicit(searchLocations.result(), tq"${baseType.typeSymbol}[$t]")
  }
}
