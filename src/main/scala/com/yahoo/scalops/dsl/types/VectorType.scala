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


import com.yahoo.scalops.examples.Vector

class VectorType extends Type[Vector] {

  case class BinaryVectorExpression(x: VectorType, y: VectorType, oper: Operator) extends VectorType {
    override def toString() = x.toString() + oper.toString() + y.toString()
  }

  case class BinaryVectorNumericExpression[T](x: VectorType, y: NumericType[T], oper: Operator) extends VectorType {
    override def toString() = x.toString() + oper.toString() + y.toString()
  }

  def +(that: VectorType) = BinaryVectorExpression(this, that, Plus)

  def -(that: VectorType) = BinaryVectorExpression(this, that, Minus)

  def *(that: VectorType) = BinaryVectorExpression(this, that, Mult)

  def *[T](that: NumericType[T]) = BinaryVectorNumericExpression[T](this, that, Mult)

  def /[T](that: NumericType[T]) = BinaryVectorNumericExpression[T](this, that, Div)

}

trait VectorImplicits {

  private[VectorImplicits] case class VectorContainer(override val value: Option[Vector]) extends VectorType

  implicit def toVector(t: Type[Vector]): VectorType = new VectorContainer(t.value)

  implicit def asVector(v: VectorType): Vector = v.value.get

}

object VectorType extends VectorImplicits {

  class Zeros(val size: Int) extends VectorType

  /**
   * Creates an empty vector of the given size
   */
  def zeros(size: Int): VectorType = new Zeros(size)

}
