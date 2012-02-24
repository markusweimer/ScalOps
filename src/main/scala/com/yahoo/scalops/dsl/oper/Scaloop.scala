/*
 * Copyright (c) 2012 Yahoo! Inc. All rights reserved. Licensed under the
 * Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
 * or agreed to in writing, software distributed under the License is distributed
 * on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 * express or implied. See the License for the specific language governing
 * permissions and limitations under the License. See accompanying LICENSE file.
 */

package com.yahoo.scalops.dsl.oper

import com.yahoo.scalops.dsl.types.Type

/**
 * Looping operations for scalop
 *
 * @author Markus Weimer <weimer@yahoo-inc.com>
 */

/**
 * A loop with a while condition
 */
case class ConditionedLoopOperator[T <: Type[_]](env: T, whileCondition: T => Type[Boolean], body: T => T) {
  override def toString = "loop(" + env + ", " + whileCondition + "){\n\t" + body(env) + "\n}"
}

/**
 * A loop over a range
 */
case class RangeLoopOperator[T <: Type[_]](env: T, range: Range, body: T => T) {
  override def toString = "loop(" + env + "," + range + "){\n\t" + body(env).getClass().toString() + "\n}"
}

trait ScaloopImplicits {

  implicit def scaloopConditionOperatorToEnvironment[T <: Type[_]](so: ConditionedLoopOperator[T]): T = so.env

  def loop[T <: Type[_]](env: T, whileCondition: T => Type[Boolean])(body: T => T) = new ConditionedLoopOperator[T](env, whileCondition, body)

  implicit def scaloopRangeOperatorToEnvironment[T <: Type[_]](so: RangeLoopOperator[T]): T = so.env

  def loop[T <: Type[_]](env: T, range: Range)(body: T => T) = new RangeLoopOperator[T](env, range, body)
}