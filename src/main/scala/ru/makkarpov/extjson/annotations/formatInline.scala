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

package ru.makkarpov.extjson.annotations

import scala.annotation.StaticAnnotation

/**
  * Has effect only on case classes with single field. When specified, serializer does not generate case-class wrapper,
  * field is serialized directly:
  *
  * <pre>
  *   @formatInline
  *   case class A(a: String)
  *   implicit val aFormat = Json.generate[A]
  *
  *   Json.toJson(A("123")).toString == "\"123\""
  * </pre>
  *
  * Name `formatInline` was chosen to avoid confusion with `@scala.inline`
  */
case class formatInline() extends StaticAnnotation
