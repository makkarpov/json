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
  * `Json` macros will refuse to generate formatter for types with this annotations, with only exception:
  * if you explicitly ask it to do so, e.g.:
  *
  * <pre>
  *   @requireImplicit
  *   case class Something(field1: String, ...)
  *
  *   implicit val somethingFormat = Json.generate[Something] // works
  * </pre>
  *
  * It is useful to make sure that recursive generation will not go too deep and will use existing implicits.
  */
case class requireImplicit() extends StaticAnnotation