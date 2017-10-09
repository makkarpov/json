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

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json._
import ru.makkarpov.extjson.JsonSuite._
import ru.makkarpov.extjson.annotations._

object JsonSuite {
  case class Test1(a: String, b: Int, c: Boolean)
  case class Test2(@key("first") a: String, @key("second") b: String, @key("third") c: String)
  case class Test3(@key("first") a: String, second: String = "qwerty")

  @serializeDefaults
  case class Test4(@key("first") a: String, second: String = "qwerty")

  case class Test5(a: String = "a", @serializeDefaults b: String = "b")

  @formatInline
  case class Test6(a: String)

  @formatInline
  case class Test7(a: String, b: String)

  @requireImplicit
  case class Test8(x: String)

  case class Test9(x: Test8)

  sealed trait Test10
  object Test10 {
    case class VInt(i: Int) extends Test10
    case class VStr(s: String) extends Test10
    case class VBool(b: Boolean) extends Test10
  }

  sealed trait Test11
  object Test11 {
    @formatInline
    case class VInt(i: Int) extends Test11

    @formatInline
    case class VStr(s: String) extends Test11

    @formatInline @fallbackCase
    case class VOther(d: JsValue) extends Test11
  }

  sealed trait Test12
  object Test12 {
    @fallbackCase
    case class Some1(i: Int) extends Test12

    @fallbackCase
    case class Some2(i: Int) extends Test12
  }

  @typeField("kind")
  sealed trait Test13
  object Test13 {
    @typeValue("ordinary")
    case class A(s: String) extends Test13

    @typeValue("strange")
    case class B(i: Int) extends Test13
  }

  sealed trait Test14
  object Test14 {
    @typeValue("same")
    case class A(s: String) extends Test14

    @typeValue("same")
    case class B(s: String) extends Test14
  }

  case class Test15(a: String, b: String)
  object Test15 {
    implicit val kser = new KeyFormat[Test15] {
      override def write(t: Test15): String = t.a + "/" + t.b
      override def read(s: String): Either[JsError, Test15] = s.split("/") match {
        case Array(a, b) => Right(Test15(a, b))
        case _ => Left(JsError("expected a/b string"))
      }
    }
  }

  case object Test16 extends Enumeration {
    val First   = Value("first")
    val Second  = Value("second")
    val Third   = Value("third")
  }

  case class Test17(a: Option[Int], b: Option[Int], c: Option[Int])

  @plainOption
  case class Test18(a: Option[Int], b: Option[Int])

  case class Test19(@plainOption a: Option[Int], b: Option[Int])

  @formatInline
  sealed trait Test20
  object Test20 {
    @fallbackCase @formatInline
    case class VOther(x: JsValue) extends Test20
    case class VInt(x: Int) extends Test20
    case class VStr(x: String) extends Test20
  }

  @addSubclass[Test21.NotSubclass]
  sealed trait Test21
  object Test21 {
    case class NotSubclass(x: Int)
  }

  // 22+ field case classes
  case class Test22(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int,
                    m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int, u: Int, v: Int, w: Int, x: Int,
                    y: Int, z: Int)

  case class Test23S(x: String, y: Int)
  case class Test23(tuple: (Int, Test23S))

  // Directly recursive
  case class Test24(v: Int, @key("n") next: Option[Test24])

  // Indirectly recursive
  case class Test25S(x: String, y: Option[Test25])
  case class Test25(x: Int, y: Test25S)
}

class JsonSuite extends WordSpec with Matchers {
  "JSON generator" must {
    "format simple case classes" in {
      implicit val format = Json.generate[Test1]

      Json.toJson(Test1("hello", 123, true)).toString shouldBe "{\"a\":\"hello\",\"b\":123,\"c\":true}"

      Json.parse("{\"a\":\"1\",\"b\":2,\"c\":false}").as[Test1] shouldBe Test1("1", 2, false)
      Json.parse("{}").asOpt[Test1] shouldBe None
      Json.parse("{\"a\":1,\"b\":2,\"c\":false}").asOpt[Test1] shouldBe None
    }

    "format case classes with overridden keys" in {
      implicit val format = Json.generate[Test2]

      Json.toJson(Test2("hello", "json", "world")).toString shouldBe
        "{\"first\":\"hello\",\"second\":\"json\",\"third\":\"world\"}"

      Json.parse("{\"first\":\"a\",\"second\":\"b\",\"third\":\"c\"}").as[Test2] shouldBe Test2("a", "b", "c")
    }

    "format case classes with default arguments" in {
      implicit val format = Json.generate[Test3]

      Json.parse("{\"first\":\"world\"}").as[Test3] shouldBe Test3("world")
      Json.parse("{}").asOpt[Test3] shouldBe None
      Json.parse("{\"first\":\"hello\",\"second\":\"world\"}").as[Test3] shouldBe Test3("hello", "world")

      Json.toJson(Test3("hello")).toString shouldBe "{\"first\":\"hello\"}"
      Json.toJson(Test3("hello", "world")).toString shouldBe "{\"first\":\"hello\",\"second\":\"world\"}"
      Json.toJson(Test3("test", "qwerty")).toString shouldBe "{\"first\":\"test\"}"
    }

    "respect per-class @serializeDefaults" in {
      implicit val format = Json.generate[Test4]

      Json.toJson(Test4("ytrewq")).toString shouldBe "{\"first\":\"ytrewq\",\"second\":\"qwerty\"}"
    }

    "respect per-field @serializeDefaults" in {
      implicit val format = Json.generate[Test5]

      Json.toJson(Test5()).toString shouldBe "{\"b\":\"b\"}"
      Json.toJson(Test5("c")).toString shouldBe "{\"a\":\"c\",\"b\":\"b\"}"
      Json.toJson(Test5(b = "c")).toString shouldBe "{\"b\":\"c\"}"
    }

    "handle @formatInline" in {
      implicit val format = Json.generate[Test6]

      Json.toJson(Test6("123")).toString shouldBe "\"123\""
      Json.parse("\"qwerty\"").as[Test6] shouldBe Test6("qwerty")
    }

    "reject @formatInline for classes with multiple arguments" in {
      "Json.generate[Test1]" should compile
      "Json.generate[Test7]" shouldNot typeCheck
    }

    "handle 22+ field case classes" in {
      implicit val format = Json.generate[Test22]

      val test = Test22(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1)
      Json.toJson(test).as[Test22] shouldBe test
    }

    "handle @requireImplicit" in {
      "Json.generate[Test1]" should compile
      "Json.generate[Test9]" shouldNot typeCheck

      implicit val fmt = Json.generate[Test8]
      implicit val fmt1 = Json.generate[Test9]

      Json.toJson(Test9(Test8("hello"))).toString shouldBe "{\"x\":{\"x\":\"hello\"}}"
      Json.parse("{\"x\":{\"x\":\"scala\"}}").as[Test9] shouldBe Test9(Test8("scala"))
    }

    "handle sealed traits/classes" in {
      implicit val fmt = Json.generate[Test10]
      import Test10._

      Json.toJson(VInt(123)).toString shouldBe "{\"i\":123,\"type\":\"VInt\"}"
      Json.toJson(VStr("qwe")).toString shouldBe "{\"s\":\"qwe\",\"type\":\"VStr\"}"
      Json.toJson(VBool(true)).toString shouldBe "{\"b\":true,\"type\":\"VBool\"}"

      Json.parse("{\"type\":\"VInt\",\"i\":456}").as[Test10] shouldBe VInt(456)
      Json.parse("{\"type\":\"VStr\",\"s\":\"test\"}").as[Test10] shouldBe VStr("test")
      Json.parse("{\"type\":\"VBool\",\"b\":true}").as[Test10] shouldBe VBool(true)
      Json.parse("{\"type\":\"VStr\",\"value\":{\"s\":\"nested\"}}").as[Test10] shouldBe VStr("nested")

      Json.parse("{\"type\":\"other\"}").asOpt[Test10] shouldBe None
      Json.parse("{\"type\":\"VStr\"}").asOpt[Test10] shouldBe None
    }

    "handle inline sealed classes & fallbacks" in {
      implicit val fmt = Json.generate[Test11]
      import Test11._

      Json.toJson(VStr("123")).toString shouldBe "{\"type\":\"VStr\",\"value\":\"123\"}"
      Json.toJson(VInt(456)).toString shouldBe "{\"type\":\"VInt\",\"value\":456}"

      Json.parse("{\"type\":\"VStr\",\"value\":\"something\"}").as[Test11] shouldBe VStr("something")
      Json.parse("{\"type\":\"fgfsa\",\"value\":\"asd\"}").as[Test11] shouldBe VOther(JsString("asd"))
    }

    "reject multiple fallback cases" in {
      "Json.generate[Test1]" should compile
      "Json.generate[Test12]" shouldNot typeCheck
    }

    "handle custom type fields & values" in {
      implicit val fmt = Json.generate[Test13]
      import Test13._

      Json.toJson(A("qwerty")).toString shouldBe "{\"s\":\"qwerty\",\"kind\":\"ordinary\"}"
      Json.toJson(B(123)).toString shouldBe "{\"i\":123,\"kind\":\"strange\"}"

      Json.parse("{\"kind\":\"ordinary\",\"s\":\"test\"}").as[Test13] shouldBe A("test")
      Json.parse("{\"kind\":\"strange\",\"i\":456}").as[Test13] shouldBe B(456)
    }

    "reject multiple type values" in {
      "Json.generate[Test1]" should compile
      "Json.generate[Test14]" shouldNot typeCheck
    }

    "handle untagged sealed classes" in {
      import Test20._
      implicit val fmt = Json.generate[Test20]

      Json.toJson(VStr("test")).toString shouldBe "{\"x\":\"test\"}"
      Json.toJson(VInt(123)).toString shouldBe "{\"x\":123}"
      Json.toJson(VOther(Json.obj("x" -> Json.arr(1, 2, 3), "y" -> "hello"))).toString shouldBe
        "{\"x\":[1,2,3],\"y\":\"hello\"}"

      Json.parse("{\"x\":\"qwe\"}").as[Test20] shouldBe VStr("qwe")
      Json.parse("{\"x\":123}").as[Test20] shouldBe VInt(123)
      Json.parse("{}").as[Test20] shouldBe VOther(Json.obj())
      Json.parse("{\"x\":{}}").as[Test20] shouldBe VOther(Json.obj("x" -> Json.obj()))
    }

    "reject @addSubclass with non-subclasses" in {
      "Json.generate[Test1]" should compile
      "Json.generate[Test21]" shouldNot typeCheck
    }

    "generate Seqs and Sets" in {
      implicit val fmt = Json.generate[Seq[Test1]]

      Json.toJson(Seq.empty).toString shouldBe "[]"

      Json.toJson(Seq(
        Test1("qwe", 123, true),
        Test1("rty", 456, false)
      )).toString shouldBe "[{\"a\":\"qwe\",\"b\":123,\"c\":true},{\"a\":\"rty\",\"b\":456,\"c\":false}]"

      Json.parse("[]").as[Seq[Test1]] shouldBe Seq.empty

      Json.parse("[{\"a\":\"1\",\"b\":2,\"c\":false},{\"a\":\"x\",\"b\":3,\"c\":true}]").as[Seq[Test1]] shouldBe
        Seq(Test1("1", 2, false), Test1("x", 3, true))

      implicit val fmt1 = Json.generate[Set[Test1]]

      Json.toJson(Set.empty[Test1]).toString shouldBe "[]"
      Json.toJson(Set(
        Test1("asd", 321, false),
        Test1("cxz", 654, true)
      )).toString shouldBe "[{\"a\":\"asd\",\"b\":321,\"c\":false},{\"a\":\"cxz\",\"b\":654,\"c\":true}]"

      Json.parse("[{\"a\":\"asd\",\"b\":321,\"c\":false},{\"a\":\"cxz\",\"b\":654,\"c\":true}]")
        .as[Set[Test1]] shouldBe Set(Test1("cxz", 654, true), Test1("asd", 321, false))
    }

    "generate Maps with string keys" in {
      implicit val fmt = Json.generate[Map[String, Test6]]

      Json.toJson(Map.empty[String, Test6]).toString shouldBe "{}"

      Json.toJson(Map(
        "hello" -> Test6("world"),
        "json" -> Test6("test")
      )).toString shouldBe "{\"hello\":\"world\",\"json\":\"test\"}"

      Json.parse("{}").as[Map[String, Test6]] shouldBe Map.empty

      Json.parse("{\"x\":\"y\",\"a\":\"b\"}").as[Map[String, Test6]] shouldBe
        Map("a" -> Test6("b"), "x" -> Test6("y"))

      Json.parse("{\"x\":\"y\",\"y\":123}").asOpt[Map[String, Test6]] shouldBe None
    }

    "generate Maps with simple keys" in {
      implicit val fmt = Json.generate[Map[Int, Test6]]

      Json.toJson(Map(123 -> Test6("one-two-three"))).toString shouldBe "{\"123\":\"one-two-three\"}"
      Json.parse("{\"1\":\"2\"}").as[Map[Int, Test6]] shouldBe Map(1 -> Test6("2"))
      Json.parse("{\"qwe\":\"2\"}").asOpt[Map[Int, Test6]] shouldBe None
    }

    "generate Maps with custom keys" in {
      implicit val fmt = Json.generate[Map[Test15, Test6]]

      Json.toJson(Map(Test15("qwe", "asd") -> Test6("xyz"))).toString shouldBe "{\"qwe/asd\":\"xyz\"}"
    }

    "generate Enumeration's" in {
      import Test16.{ValueSet, Value}
      implicit val fmt = Json.generate[Value]

      Json.toJson(Test16.Second).toString shouldBe "\"second\""
      Json.parse("\"third\"").as[Value] shouldBe Test16.Third
      Json.parse("\"qwe\"").asOpt[Value] shouldBe None

      implicit val fmt1 = Json.generate[ValueSet]

      Json.toJson(Test16.values).toString shouldBe "[\"first\",\"second\",\"third\"]"
      Json.parse("[]").as[ValueSet] shouldBe Test16.ValueSet()
      Json.parse("[\"first\",\"third\"]").as[ValueSet] shouldBe (Test16.First + Test16.Third)
      Json.parse("[\"first\",\"x\"]").asOpt[ValueSet] shouldBe None
    }

    "handle standalone Options" in {
      implicit val fmt = Json.generate[Option[String]]

      Json.toJson(None).toString shouldBe "[]"
      Json.toJson(Some("test")).toString shouldBe "[\"test\"]"

      Json.parse("[]").as[Option[String]] shouldBe None
      Json.parse("[\"qwerty\"]").as[Option[String]] shouldBe Some("qwerty")
      Json.parse("[\"1\",\"2\"]").asOpt[Option[String]] shouldBe None
    }

    "handle Options in case classes" in {
      implicit val fmt = Json.generate[Test17]

      Json.toJson(Test17(None, None, None)).toString shouldBe "{}"
      Json.toJson(Test17(Some(123), None, None)).toString shouldBe "{\"a\":123}"
      Json.toJson(Test17(None, None, Some(456))).toString shouldBe "{\"c\":456}"
      Json.toJson(Test17(Some(123), None, Some(456))).toString shouldBe "{\"a\":123,\"c\":456}"

      Json.parse("{}").as[Test17] shouldBe Test17(None, None, None)
      Json.parse("{\"b\":123}").as[Test17] shouldBe Test17(None, Some(123), None)
      Json.parse("{\"b\":\"qwe\"}").asOpt[Test17] shouldBe None
    }

    "respect per-class @plainOption" in {
      implicit val fmt = Json.generate[Test18]

      Json.toJson(Test18(None, None)).toString shouldBe "{\"a\":[],\"b\":[]}"
      Json.toJson(Test18(Some(123), None)).toString shouldBe "{\"a\":[123],\"b\":[]}"

      Json.parse("{}").asOpt[Test18] shouldBe None
      Json.parse("{\"a\":[],\"b\":[456]}").as[Test18] shouldBe Test18(None, Some(456))
    }

    "respect per-field @plainOption" in {
      implicit val fmt = Json.generate[Test19]

      Json.toJson(Test19(None, None)).toString shouldBe "{\"a\":[]}"
      Json.toJson(Test19(Some(123), None)).toString shouldBe "{\"a\":[123]}"
      Json.toJson(Test19(None, Some(456))).toString shouldBe "{\"a\":[],\"b\":456}"

      Json.parse("{}").asOpt[Test19] shouldBe None
      Json.parse("{\"b\":456}").asOpt[Test19] shouldBe None
      Json.parse("{\"a\":[]}").as[Test19] shouldBe Test19(None, None)
      Json.parse("{\"a\":[123]}").as[Test19] shouldBe Test19(Some(123), None)
      Json.parse("{\"a\":123}").asOpt[Test19] shouldBe None
    }

    "handle plain tuples" in {
      implicit val fmt = Json.generate[(String, Int, Boolean)]

      Json.toJson(("test", 123, true)).toString shouldBe "[\"test\",123,true]"
      Json.parse("[\"123\",456,false]").as[(String, Int, Boolean)] shouldBe ("123", 456, false)
      Json.parse("[]").asOpt[(String, Int, Boolean)] shouldBe None
      Json.parse("[123,456,false]").asOpt[(String, Int, Boolean)] shouldBe None
      Json.parse("[\"123\",456,false,123]").asOpt[(String, Int, Boolean)] shouldBe None
    }

    "handle nested tuples" in {
      implicit val fmt = Json.generate[Test23]

      Json.toJson(Test23(123 -> Test23S("some", 456))).toString shouldBe "{\"tuple\":[123,{\"x\":\"some\",\"y\":456}]}"
      Json.parse("{\"tuple\":[456,{\"x\":\"abc\",\"y\":321}]}").as[Test23] shouldBe Test23(456 -> Test23S("abc", 321))
    }

    "handle GeneratedFormat" in {
      def test[T](t: T)(implicit fmt: GeneratedFormat[T]) = {
        import fmt.implicitFmt
        Json.toJson(t).toString
      }

      test(Test1("a", 123, true)) shouldBe "{\"a\":\"a\",\"b\":123,\"c\":true}"
      test(Test6("qwe")) shouldBe "\"qwe\""
    }

    "handle direct recursive formats" in {
      implicit val fmt = Json.generate[Test24]

      def recur(i: Int): Test24 = if (i == 1) Test24(i, None) else Test24(i, Some(recur(i - 1)))
      def test(i: Int): String = Json.toJson(recur(i)).toString

      test(1) shouldBe "{\"v\":1}"
      test(2) shouldBe "{\"v\":2,\"n\":{\"v\":1}}"
      test(3) shouldBe "{\"v\":3,\"n\":{\"v\":2,\"n\":{\"v\":1}}}"
      test(10) shouldBe "{\"v\":10,\"n\":{\"v\":9,\"n\":{\"v\":8,\"n\":{\"v\":7,\"n\":{\"v\":6,\"n\":{\"v\":5,\"n\":{"+
                        "\"v\":4,\"n\":{\"v\":3,\"n\":{\"v\":2,\"n\":{\"v\":1}}}}}}}}}}"
    }

    "handle indirect recursive formats" in {
      implicit val fmt = Json.generate[Test25]

      def recur(i: Int): Test25 =
        if (i == 1) Test25(i, Test25S("t" + i, None)) else Test25(i, Test25S("n" + i, Some(recur(i - 1))))
      def test(i: Int): String = Json.toJson(recur(i)).toString

      test(1) shouldBe "{\"x\":1,\"y\":{\"x\":\"t1\"}}"
      test(5) shouldBe "{\"x\":5,\"y\":{\"x\":\"n5\",\"y\":{\"x\":4,\"y\":{\"x\":\"n4\",\"y\":{\"x\":3,\"y\":{\"x\":\""+
                       "n3\",\"y\":{\"x\":2,\"y\":{\"x\":\"n2\",\"y\":{\"x\":1,\"y\":{\"x\":\"t1\"}}}}}}}}}}"
    }
  }
}
