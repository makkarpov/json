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

object JsonFormats {
  implicit def optionFormat[T](implicit fmt: Format[T]): Format[Option[T]] = new Format[Option[T]] {
    override def writes(o: Option[T]) = JsArray(o.map(fmt.writes).toSeq)
    override def reads(json: JsValue): JsResult[Option[T]] = json match {
      case JsArray(Seq(x)) => fmt.reads(x).map(Some(_))
      case JsArray(Seq()) => JsSuccess(None)
      case JsArray(_) => JsError("expected jsarray with 0 or 1 elements")
      case _ => JsError("expected jsarray")
    }
  }
}
