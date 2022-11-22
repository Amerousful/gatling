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

import com.mitchellbosecke.pebble.extension.{AbstractExtension, Function}

import java.util
import scala.jdk.CollectionConverters._

class PebbleELFunction extends AbstractExtension {

  override def getFunctions: util.Map[String, Function] = {
    val functions: Map[String, _ <: Function] = Map(
      "currentTimeMillis" -> new CurrentTimeMillis,
      "currentDate" -> new CurrentDate,
      "randomUuid" -> new RandomUUID,
      "randomSecureUuid" -> new RandomSecureUUID,
      "randomInt" -> new RandomInt,
      "randomLong" -> new RandomLong
    )
    functions.asJava
  }

}
