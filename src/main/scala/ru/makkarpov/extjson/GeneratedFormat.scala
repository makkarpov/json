package ru.makkarpov.extjson

import play.api.libs.json.Format
import ru.makkarpov.extjson.generator.Macros

import scala.language.experimental.macros

object GeneratedFormat {
  implicit def materializeFormat[T]: GeneratedFormat[T] = macro Macros.materializeFormat[T]
}

/**
  * Creating an implicit materializer for `Format[T]` is pretty dangerous since it can silently bloat your code.
  * But implicit generation of formatter can be useful is some cases, so I can't ignore it completely.
  */
case class GeneratedFormat[T](format: Format[T]) {
  implicit val implicitFmt: Format[T] = format
}
