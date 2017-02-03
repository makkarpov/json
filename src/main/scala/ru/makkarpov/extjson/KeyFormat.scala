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

import play.api.libs.json.JsError

object KeyFormat {
  implicit object StringKeyFormatter extends KeyFormat[String] {
    override def write(t: String): String = t
    override def read(s: String): Either[JsError, String] = Right(s)
  }

  implicit object IntKeyFormatter extends KeyFormat[Int] {
    override def write(t: Int): String = t.toString
    override def read(s: String): Either[JsError, Int] =
      try Right(s.toInt) catch { case _: NumberFormatException => Left(JsError("malformed number"))}
  }
}

/**
  * A formatter for `Map` keys. Since JSON only allows string keys, a separate formatter is needed.
  *
  * @tparam T Key type
  */
trait KeyFormat[T] {
  def write(t: T): String
  def read(s: String): Either[JsError, T]
}
