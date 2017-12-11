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

object StrFormat {
  implicit object StringStrFormatter extends StrFormat[String] {
    override def write(t: String): String = t
    override def read(s: String): Either[JsError, String] = Right(s)
  }

  implicit object IntStrFormatter extends StrFormat[Int] {
    override def write(t: Int): String = t.toString
    override def read(s: String): Either[JsError, Int] =
      try Right(s.toInt) catch { case _: NumberFormatException => Left(JsError("malformed int")) }
  }

  implicit object LongStrFormatter extends StrFormat[Long] {
    override def write(t: Long): String = t.toString
    override def read(s: String): Either[JsError, Long] =
      try Right(s.toLong) catch { case _: NumberFormatException => Left(JsError("malformed long")) }
  }

  implicit object FloatStrFormatter extends StrFormat[Float] {
    override def write(t: Float): String = t.toString
    override def read(s: String): Either[JsError, Float] =
      try Right(s.toFloat) catch { case _: NumberFormatException => Left(JsError("malformed float")) }
  }

  implicit object DoubleStrFormatter extends StrFormat[Double] {
    override def write(t: Double): String = t.toString
    override def read(s: String): Either[JsError, Double] =
      try Right(s.toDouble) catch { case _: NumberFormatException => Left(JsError("malformed double")) }
  }

  implicit object BigIntStrFormatter extends StrFormat[BigInt] {
    override def write(t: BigInt): String = t.toString
    override def read(s: String): Either[JsError, BigInt] =
      try Right(BigInt(s)) catch { case _: NumberFormatException => Left(JsError("malformed bigInt")) }
  }

  implicit object BigDecimalStrFormatter extends StrFormat[BigDecimal] {
    override def write(t: BigDecimal): String = t.toString
    override def read(s: String): Either[JsError, BigDecimal] =
      try Right(BigDecimal(s)) catch { case _: NumberFormatException => Left(JsError("malformed bigDecimal")) }
  }
}

/**
  * A formatter for `Map` keys and values marked as @asString.
  * Since JSON only allows string keys, a separate formatter is needed.
  *
  * Named `StrFormat` to avoid conflicts with `scala.Predef.StringFormat`.
  *
  * @tparam T Key type
  */
trait StrFormat[T] {
  def write(t: T): String
  def read(s: String): Either[JsError, T]

  lazy val format: Format[T] = new Format[T] {
    override def writes(o: T): JsValue = JsString(write(o))
    override def reads(json: JsValue): JsResult[T] =
      json.validate[String].flatMap(s => read(s).fold(identity, JsSuccess(_)))
  }
}
