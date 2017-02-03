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

package ru.makkarpov.extjson

import play.api.libs.json._

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

object JsonUtils {
  val valueField = "value"

  def addType(typeField: String, typeValue: String, data: JsValue): JsObject = data match {
    case JsObject(fields) => JsObject(fields + (typeField -> JsString(typeValue)))
    case other => Json.obj(typeField -> typeValue, valueField -> other)
  }

  def stripType(typeField: String, data: JsValue): JsResult[(JsValue, String)] = data match {
    case JsObject(fields) =>
      val isSingleValued = fields.keySet == Set(typeField, valueField)
      val inner = if (isSingleValued) fields("value") else JsObject(fields - typeField)
      for (t <- (data \ typeField).validate[String]) yield (inner, t)
    case _ => JsError("expected object")
  }

  def mergeErrors[T](results: JsResult[_]*)(f: => T): JsResult[T] =
    results.flatMap {
      case JsSuccess(_, _) => Nil
      case JsError(e) => e
    } match {
      case Seq() => JsSuccess(f)
      case x => JsError(x)
    }

  def traversableFormat[F[X] <: Traversable[X], A](bf: CanBuildFrom[F[_], A, F[A]], fa: Format[A]): Format[F[A]] = {
    val reads = Reads.traversableReads[F, A](bf, fa)
    val writes = Writes.traversableWrites[A](fa)

    Format(reads, writes)
  }

  def mapFormat[M[X, Y] <: Map[X, Y], K, V](fk: KeyFormat[K], fv: Format[V],
                                            cbf: CanBuildFrom[M[_, _], (K, V), M[K, V]]): Format[M[K, V]] =
    new Format[M[K, V]] {
      override def reads(json: JsValue): JsResult[M[K, V]] = json match {
        case JsObject(fields) =>
          val bld = cbf()

          for ((sk, sv) <- fields)
            fk.read(sk) match {
              case Left(err) => return err.repath(__ \ sk)
              case Right(k) => fv.reads(sv) match {
                case JsSuccess(v, _) => bld += k -> v
                case err: JsError => return err.repath(__ \ sk)
              }
            }

          JsSuccess(bld.result())
        case _ => JsError("expected JsObject")
      }

      override def writes(o: M[K, V]): JsValue = {
        val ret = Map.newBuilder[String, JsValue]

        for ((k, v) <- o)
          ret += fk.write(k) -> fv.writes(v)

        JsObject(ret.result())
      }
    }

  def enumerationFormat[E <: Enumeration](inst: E): Format[inst.Value] = new Format[inst.Value] {
    override def writes(o: inst.Value): JsValue = JsString(o.toString)
    override def reads(json: JsValue): JsResult[inst.Value] = json match {
      case JsString(str) => inst.values.find(_.toString == str) match {
        case Some(x) => JsSuccess(x)
        case None => JsError("undefined value")
      }
      case _ => JsError("expected jsstring")
    }
  }

  def enumerationSetFormat[E <: Enumeration](inst: E): Format[inst.ValueSet] = new Format[inst.ValueSet] {
    override def writes(o: inst.ValueSet): JsValue = JsArray(o.map(x => JsString(x.toString)).toSeq)
    override def reads(json: JsValue): JsResult[inst.ValueSet] = json match {
      case JsArray(elems) =>
        var bld = inst.ValueSet()

        for ((e, i) <- elems.zipWithIndex) e match {
          case JsString(str) => inst.values.find(_.toString == str) match {
            case Some(x) => bld += x
            case None => return JsError(__ \ i, "undefined value")
          }
          case _ => return JsError(__ \ i, "expected jsstring")
        }

        JsSuccess(bld)
      case _ => JsError("expected jsarray or jsstring")
    }
  }
}
