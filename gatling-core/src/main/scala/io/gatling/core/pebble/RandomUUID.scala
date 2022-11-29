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

import java.util
import java.util.UUID
import java.util.concurrent.ThreadLocalRandom

import io.pebbletemplates.pebble.extension.Function
import io.pebbletemplates.pebble.template.{ EvaluationContext, PebbleTemplate }

class RandomUUID extends Function {
  override def getArgumentNames = null

  override def execute(args: util.Map[String, AnyRef], self: PebbleTemplate, context: EvaluationContext, lineNumber: Int) = {
    val Version4Mask = 2L << 62
    val VariantMask = 2L << 62

    def version4UUID(): UUID = {
      val rnd = ThreadLocalRandom.current()
      var mostSigBits = rnd.nextLong()
      var leastSigBits = rnd.nextLong()

      mostSigBits &= ~0xf000L
      mostSigBits |= Version4Mask

      leastSigBits = (leastSigBits << 2) >>> 2
      leastSigBits |= VariantMask
      new UUID(mostSigBits, leastSigBits)
    }
    version4UUID()
  }

}
