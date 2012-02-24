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

package com.yahoo.scalops.dsl.types

class BooleanType extends Type[Boolean] {

  case class UnaryBooleanExpression(t: Type[Boolean], oper: String) extends BooleanType

  case class BinaryBooleanExpression(b1: Type[Boolean], b2: Type[Boolean], oper: String) extends BooleanType

  def unary_! : BooleanType = new UnaryBooleanExpression(this, "!")

  def &&(that: Type[Boolean]) = new BinaryBooleanExpression(this, that, "&&")

  def ||(that: Type[Boolean]) = new BinaryBooleanExpression(this, that, "||")
}

trait BooleanImplicits {

  private[BooleanImplicits] case class BooleanContainer(override val value: Option[Boolean]) extends BooleanType

  implicit def typeToBooleanType(t: Type[Boolean]) = new BooleanContainer(t.value)

  implicit def boxBoolean(b: Boolean) = BooleanContainer(Some(b))
}

object BooleanType extends BooleanImplicits