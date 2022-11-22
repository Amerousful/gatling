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

package io.gatling.core.pebble

import java.util.Map
import java.util.concurrent.ThreadLocalRandom

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

import com.mitchellbosecke.pebble.error.PebbleException
import com.mitchellbosecke.pebble.extension.Function
import com.mitchellbosecke.pebble.template.{ EvaluationContext, PebbleTemplate }

class RandomLong extends Function {

  override def getArgumentNames = ArrayBuffer("min", "max").asJava

  override def execute(args: Map[String, AnyRef], self: PebbleTemplate, context: EvaluationContext, lineNumber: Int) =
    args.isEmpty match {
      case true => ThreadLocalRandom.current().nextInt()
      case false =>
        val min = args.get("min").toString.toLong
        val max = args.get("max").toString.toLong

        if (min > max) {
          throw new PebbleException(null, s"Range 'max'($max) must be above than 'min'($min)", lineNumber, self.getName)
        }

        val length = max - min + 1
        min + ThreadLocalRandom.current().nextLong(length)
    }

}
