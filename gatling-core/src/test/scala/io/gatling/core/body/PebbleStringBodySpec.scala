/*
 * Copyright 2011-2022 GatlingCorp (https://gatling.io)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.gatling.core.body

import java.util.UUID

import io.gatling.{ BaseSpec, ValidationValues }
import io.gatling.core.EmptySession
import io.gatling.core.config.GatlingConfiguration

class PebbleStringBodySpec extends BaseSpec with ValidationValues with EmptySession {
  private implicit val configuration: GatlingConfiguration = GatlingConfiguration.loadForTest()

  def exceptionToFail(method: => Any): Unit =
    try {
      method
    } catch {
      case e: Exception => fail(e.getMessage)
    }

  "Static String" should "return itself" in {
    val session = emptySession
    val body = PebbleStringBody("bar", configuration.core.charset)
    body(session).succeeded shouldBe "bar"
  }

  it should "return empty when empty" in {
    val session = emptySession
    val body = PebbleStringBody("", configuration.core.charset)
    body(session).succeeded shouldBe ""
  }

  "One monovalued Expression" should "return expected result when the variable is the whole string" in {
    val session = emptySession.set("bar", "BAR")
    val body = PebbleStringBody("{{bar}}", configuration.core.charset)
    body(session).succeeded shouldBe "BAR"
  }

  it should "return expected result when the variable is at the end of the string" in {
    val session = emptySession.set("bar", "BAR")
    val body = PebbleStringBody("foo{{bar}}", configuration.core.charset)
    body(session).succeeded shouldBe "fooBAR"
  }

  it should "return expected result when the variable is at the beginning of the string" in {
    val session = emptySession.set("bar", "BAR")
    val body = PebbleStringBody("{{bar}}foo", configuration.core.charset)
    body(session).succeeded shouldBe "BARfoo"
  }

  it should "return expected result when the variable is in the middle of the string" in {
    val session = emptySession.set("bar", "BAR")
    val body = PebbleStringBody("foo{{bar}}foo", configuration.core.charset)
    body(session).succeeded shouldBe "fooBARfoo"
  }

  it should "handle when an attribute is missing" in {
    val session = emptySession.set("foo", "FOO")
    val body = PebbleStringBody("foo{{bar}}", configuration.core.charset)
    body(session).succeeded shouldBe "foo"
  }

  it should "properly handle multiline JSON template" in {
    val session = emptySession.set("foo", "FOO")
    val body = PebbleStringBody(
      """{
        "foo": {{foo}},
        "bar": 1
      }""",
      configuration.core.charset
    )
    body(session).succeeded shouldBe
      """{
        "foo": FOO,
        "bar": 1
      }"""
  }

  it should "properly handle if" in {
    val session = emptySession.setAll("barTrue" -> "BARTRUE", "barFalse" -> "BARFALSE", "true" -> true, "false" -> false)
    val body = PebbleStringBody("{%if true %}{{barTrue}}{%endif%}{%if false %}{{barFalse}}{% endif %}", configuration.core.charset)
    body(session).succeeded shouldBe "BARTRUE"
  }

  it should "handle gracefully when an exception is thrown" in {
    val session = emptySession
    val body = PebbleStringBody("{{ 0 / 0 }}", configuration.core.charset)
    body(session).failed
  }

  "Multivalued Expression" should "return expected result with 2 monovalued expressions" in {
    val session = emptySession.setAll("foo" -> "FOO", "bar" -> "BAR")
    val body = PebbleStringBody("{{foo}} {{bar}}", configuration.core.charset)
    body(session).succeeded shouldBe "FOO BAR"
  }

  it should "properly handle for loop" in {
    val session = emptySession.set("list", List("hello", "bonjour", 42))
    val body = PebbleStringBody("{% for value in list %}{{value }}{% endfor %}", configuration.core.charset)
    body(session).succeeded shouldBe "hellobonjour42"
  }

  it should "support index access for Scala Seq" in {
    val session = emptySession.set("list", Seq(1, 2, 3))
    val body = PebbleStringBody("{{list[0]}}", configuration.core.charset)
    body(session).succeeded shouldBe "1"
  }

  it should "handle Maps" in {
    val session = emptySession.set("map", Map("foo" -> "bar"))
    val body = PebbleStringBody("{{map.foo}}", configuration.core.charset)
    body(session).succeeded shouldBe "bar"
  }

  it should "handle deep objects" in {
    val session = emptySession.set("map", List(Map("key" -> "key1", "value" -> "value1"), Map("key" -> "key2", "value" -> "value2")))

    val template =
      """{% for element in map %}
        |{
        |  "name": "{{element.key}}",
        |  "value": "{{element.value}}"
        |}{% if loop.last %}{% else %},
        |{% endif %}
        |{% endfor %}""".stripMargin

    val body = PebbleStringBody(template, configuration.core.charset)
    body(session).succeeded shouldBe
      """{
        |  "name": "key1",
        |  "value": "value1"
        |},
        |{
        |  "name": "key2",
        |  "value": "value2"
        |}""".stripMargin
  }

  it should "return expected result when using filters" in {
    val session = emptySession.set("bar", "bar")
    val body = PebbleStringBody("{{ bar | capitalize }}{% filter upper %}hello{% endfilter %}", configuration.core.charset)
    body(session).succeeded shouldBe "BarHELLO"
  }

  "currentTimeMillis" should "generate a long" in {
    val body = PebbleStringBody("""{{currentTimeMillis()}}""", configuration.core.charset)
    exceptionToFail(body(emptySession).succeeded.toLong)
  }

  "currentDate" should "generate a String" in {
    val body = PebbleStringBody("""{{currentDate(format="yyyy-MM-dd HH:mm:ss")}}""", configuration.core.charset)
    body(emptySession).succeeded.length shouldBe 19
  }

  "randomUuid" should "generate uuid" in {
    val body = PebbleStringBody("""{{randomUuid()}}""", configuration.core.charset)
    exceptionToFail(UUID.fromString(body(emptySession).succeeded))
  }

  "randomSecureUuid" should "generate uuid" in {
    val body = PebbleStringBody("""{{randomSecureUuid()}}""", configuration.core.charset)
    exceptionToFail(UUID.fromString(body(emptySession).succeeded))
  }

  "randomInt" should "generate random Int full range" in {
    val randomInt = PebbleStringBody("""{{randomInt()}}""", configuration.core.charset)
    randomInt(emptySession).succeeded.toInt should (be >= Int.MinValue and be <= Int.MaxValue)
  }

  "randomInt" should "generate random Int with range (inclusive)" in {
    val randomInt = PebbleStringBody("""{{randomInt(0,10)}}""", configuration.core.charset)
    val actual = Set.fill(1000)(randomInt(emptySession).succeeded.toInt)
    val expected = (0 to 10).toSet
    actual should contain theSameElementsAs expected
  }

  "randomInt" should "generate random Int with negative numbers" in {
    val randomInt = PebbleStringBody("""{{randomInt(-10,-5)}}""", configuration.core.charset)
    randomInt(emptySession).succeeded.toInt should (be < 0)
  }

  "randomInt" should "throw exception with 'max' less than 'min'" in {
    val randomInt = PebbleStringBody("""{{randomInt(20,1)}}""", configuration.core.charset)
    randomInt(emptySession).failed shouldBe "Range 'max'(1) must be above than 'min'(20) ({{randomInt(20,1)}}:1)"
  }

  "randomLong" should "generate random Long full range" in {
    val randomLong = PebbleStringBody("""{{randomLong()}}""", configuration.core.charset)
    randomLong(emptySession).succeeded.toLong should (be >= Long.MinValue and be <= Long.MaxValue)
  }

  "randomLong" should "generate random Long with range (inclusive)" in {
    val randomLong = PebbleStringBody("""{{randomLong(2147483648,2147483658)}}""", configuration.core.charset)
    val actual = Set.fill(1000)(randomLong(emptySession).succeeded.toLong)
    val expected = (Int.MaxValue + 1L to Int.MaxValue + 11L).toSet
    actual should contain theSameElementsAs expected
  }

  "randomLong" should "generate random Long with negative numbers" in {
    val randomLong = PebbleStringBody("""{{randomLong(-2147483658,-2147483648)}}""", configuration.core.charset)
    randomLong(emptySession).succeeded.toLong should (be < -2147483647L)
  }

  "randomLong" should "throw exception with 'max' less than 'min'" in {
    val randomLong = PebbleStringBody("""{{randomLong(2147483658,2147483648)}}""", configuration.core.charset)
    randomLong(emptySession).failed shouldBe "Range 'max'(2147483648) must be above than 'min'(2147483658) ({{randomLong(2147483658,2147483648)}}:1)"
  }

}
