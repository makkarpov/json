// Generated file
package ru.makkarpov.extjson

import play.api.libs.json._

object Tuples {
  class Tuple2Format[T1,T2](fmt1:Format[T1],fmt2:Format[T2])
    extends Format[(T1,T2)] {
    def writes(t: (T1,T2)): JsValue = JsArray(Seq(fmt1.writes(t._1),fmt2.writes(t._2)))

    def reads(j: JsValue): JsResult[(T1,T2)] = j match {
      case JsArray(Seq(p1, p2)) =>
        val r1 = fmt1.reads(p1)
        val r2 = fmt2.reads(p2)
        if (r1.isSuccess&&r2.isSuccess) JsSuccess((r1.get,r2.get))
        else JsonUtils.mergeErrors(r1,r2)
      case _ => JsError("jsArray expected")
    }
  }

  class Tuple3Format[T1,T2,T3](fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3])
    extends Format[(T1,T2,T3)] {
    def writes(t: (T1,T2,T3)): JsValue = JsArray(Seq(fmt1.writes(t._1),fmt2.writes(t._2),fmt3.writes(t._3)))

    def reads(j: JsValue): JsResult[(T1,T2,T3)] = j match {
      case JsArray(Seq(p1, p2, p3)) =>
        val r1 = fmt1.reads(p1)
        val r2 = fmt2.reads(p2)
        val r3 = fmt3.reads(p3)
        if (r1.isSuccess&&r2.isSuccess&&r3.isSuccess) JsSuccess((r1.get,r2.get,r3.get))
        else JsonUtils.mergeErrors(r1,r2,r3)
      case _ => JsError("jsArray expected")
    }
  }

  class Tuple4Format[T1,T2,T3,T4](fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4])
    extends Format[(T1,T2,T3,T4)] {
    def writes(t: (T1,T2,T3,T4)): JsValue = JsArray(Seq(fmt1.writes(t._1),fmt2.writes(t._2),fmt3.writes(t._3),fmt4.writes(t._4)))

    def reads(j: JsValue): JsResult[(T1,T2,T3,T4)] = j match {
      case JsArray(Seq(p1, p2, p3, p4)) =>
        val r1 = fmt1.reads(p1)
        val r2 = fmt2.reads(p2)
        val r3 = fmt3.reads(p3)
        val r4 = fmt4.reads(p4)
        if (r1.isSuccess&&r2.isSuccess&&r3.isSuccess&&r4.isSuccess) JsSuccess((r1.get,r2.get,r3.get,r4.get))
        else JsonUtils.mergeErrors(r1,r2,r3,r4)
      case _ => JsError("jsArray expected")
    }
  }

  class Tuple5Format[T1,T2,T3,T4,T5](fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4],fmt5:Format[T5])
    extends Format[(T1,T2,T3,T4,T5)] {
    def writes(t: (T1,T2,T3,T4,T5)): JsValue = JsArray(Seq(fmt1.writes(t._1),fmt2.writes(t._2),fmt3.writes(t._3),fmt4.writes(t._4),fmt5.writes(t._5)))

    def reads(j: JsValue): JsResult[(T1,T2,T3,T4,T5)] = j match {
      case JsArray(Seq(p1, p2, p3, p4, p5)) =>
        val r1 = fmt1.reads(p1)
        val r2 = fmt2.reads(p2)
        val r3 = fmt3.reads(p3)
        val r4 = fmt4.reads(p4)
        val r5 = fmt5.reads(p5)
        if (r1.isSuccess&&r2.isSuccess&&r3.isSuccess&&r4.isSuccess&&r5.isSuccess) JsSuccess((r1.get,r2.get,r3.get,r4.get,r5.get))
        else JsonUtils.mergeErrors(r1,r2,r3,r4,r5)
      case _ => JsError("jsArray expected")
    }
  }

  class Tuple6Format[T1,T2,T3,T4,T5,T6](fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4],fmt5:Format[T5],fmt6:Format[T6])
    extends Format[(T1,T2,T3,T4,T5,T6)] {
    def writes(t: (T1,T2,T3,T4,T5,T6)): JsValue = JsArray(Seq(fmt1.writes(t._1),fmt2.writes(t._2),fmt3.writes(t._3),fmt4.writes(t._4),fmt5.writes(t._5),fmt6.writes(t._6)))

    def reads(j: JsValue): JsResult[(T1,T2,T3,T4,T5,T6)] = j match {
      case JsArray(Seq(p1, p2, p3, p4, p5, p6)) =>
        val r1 = fmt1.reads(p1)
        val r2 = fmt2.reads(p2)
        val r3 = fmt3.reads(p3)
        val r4 = fmt4.reads(p4)
        val r5 = fmt5.reads(p5)
        val r6 = fmt6.reads(p6)
        if (r1.isSuccess&&r2.isSuccess&&r3.isSuccess&&r4.isSuccess&&r5.isSuccess&&r6.isSuccess) JsSuccess((r1.get,r2.get,r3.get,r4.get,r5.get,r6.get))
        else JsonUtils.mergeErrors(r1,r2,r3,r4,r5,r6)
      case _ => JsError("jsArray expected")
    }
  }

  class Tuple7Format[T1,T2,T3,T4,T5,T6,T7](fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4],fmt5:Format[T5],fmt6:Format[T6],fmt7:Format[T7])
    extends Format[(T1,T2,T3,T4,T5,T6,T7)] {
    def writes(t: (T1,T2,T3,T4,T5,T6,T7)): JsValue = JsArray(Seq(fmt1.writes(t._1),fmt2.writes(t._2),fmt3.writes(t._3),fmt4.writes(t._4),fmt5.writes(t._5),fmt6.writes(t._6),fmt7.writes(t._7)))

    def reads(j: JsValue): JsResult[(T1,T2,T3,T4,T5,T6,T7)] = j match {
      case JsArray(Seq(p1, p2, p3, p4, p5, p6, p7)) =>
        val r1 = fmt1.reads(p1)
        val r2 = fmt2.reads(p2)
        val r3 = fmt3.reads(p3)
        val r4 = fmt4.reads(p4)
        val r5 = fmt5.reads(p5)
        val r6 = fmt6.reads(p6)
        val r7 = fmt7.reads(p7)
        if (r1.isSuccess&&r2.isSuccess&&r3.isSuccess&&r4.isSuccess&&r5.isSuccess&&r6.isSuccess&&r7.isSuccess) JsSuccess((r1.get,r2.get,r3.get,r4.get,r5.get,r6.get,r7.get))
        else JsonUtils.mergeErrors(r1,r2,r3,r4,r5,r6,r7)
      case _ => JsError("jsArray expected")
    }
  }

  class Tuple8Format[T1,T2,T3,T4,T5,T6,T7,T8](fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4],fmt5:Format[T5],fmt6:Format[T6],fmt7:Format[T7],fmt8:Format[T8])
    extends Format[(T1,T2,T3,T4,T5,T6,T7,T8)] {
    def writes(t: (T1,T2,T3,T4,T5,T6,T7,T8)): JsValue = JsArray(Seq(fmt1.writes(t._1),fmt2.writes(t._2),fmt3.writes(t._3),fmt4.writes(t._4),fmt5.writes(t._5),fmt6.writes(t._6),fmt7.writes(t._7),fmt8.writes(t._8)))

    def reads(j: JsValue): JsResult[(T1,T2,T3,T4,T5,T6,T7,T8)] = j match {
      case JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8)) =>
        val r1 = fmt1.reads(p1)
        val r2 = fmt2.reads(p2)
        val r3 = fmt3.reads(p3)
        val r4 = fmt4.reads(p4)
        val r5 = fmt5.reads(p5)
        val r6 = fmt6.reads(p6)
        val r7 = fmt7.reads(p7)
        val r8 = fmt8.reads(p8)
        if (r1.isSuccess&&r2.isSuccess&&r3.isSuccess&&r4.isSuccess&&r5.isSuccess&&r6.isSuccess&&r7.isSuccess&&r8.isSuccess) JsSuccess((r1.get,r2.get,r3.get,r4.get,r5.get,r6.get,r7.get,r8.get))
        else JsonUtils.mergeErrors(r1,r2,r3,r4,r5,r6,r7,r8)
      case _ => JsError("jsArray expected")
    }
  }

}

trait Tuples {
  implicit def tuple2Format[T1,T2](implicit fmt1:Format[T1],fmt2:Format[T2]): Format[(T1,T2)] =
    new Tuples.Tuple2Format[T1,T2](fmt1,fmt2)
  implicit def tuple3Format[T1,T2,T3](implicit fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3]): Format[(T1,T2,T3)] =
    new Tuples.Tuple3Format[T1,T2,T3](fmt1,fmt2,fmt3)
  implicit def tuple4Format[T1,T2,T3,T4](implicit fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4]): Format[(T1,T2,T3,T4)] =
    new Tuples.Tuple4Format[T1,T2,T3,T4](fmt1,fmt2,fmt3,fmt4)
  implicit def tuple5Format[T1,T2,T3,T4,T5](implicit fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4],fmt5:Format[T5]): Format[(T1,T2,T3,T4,T5)] =
    new Tuples.Tuple5Format[T1,T2,T3,T4,T5](fmt1,fmt2,fmt3,fmt4,fmt5)
  implicit def tuple6Format[T1,T2,T3,T4,T5,T6](implicit fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4],fmt5:Format[T5],fmt6:Format[T6]): Format[(T1,T2,T3,T4,T5,T6)] =
    new Tuples.Tuple6Format[T1,T2,T3,T4,T5,T6](fmt1,fmt2,fmt3,fmt4,fmt5,fmt6)
  implicit def tuple7Format[T1,T2,T3,T4,T5,T6,T7](implicit fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4],fmt5:Format[T5],fmt6:Format[T6],fmt7:Format[T7]): Format[(T1,T2,T3,T4,T5,T6,T7)] =
    new Tuples.Tuple7Format[T1,T2,T3,T4,T5,T6,T7](fmt1,fmt2,fmt3,fmt4,fmt5,fmt6,fmt7)
  implicit def tuple8Format[T1,T2,T3,T4,T5,T6,T7,T8](implicit fmt1:Format[T1],fmt2:Format[T2],fmt3:Format[T3],fmt4:Format[T4],fmt5:Format[T5],fmt6:Format[T6],fmt7:Format[T7],fmt8:Format[T8]): Format[(T1,T2,T3,T4,T5,T6,T7,T8)] =
    new Tuples.Tuple8Format[T1,T2,T3,T4,T5,T6,T7,T8](fmt1,fmt2,fmt3,fmt4,fmt5,fmt6,fmt7,fmt8)
}