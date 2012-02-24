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

case class NumericType[T](override val value: Option[T])(implicit numeric: Numeric[T]) extends Type[T] {

  case class BinaryNumericExpression[T](x: NumericType[T], y: NumericType[T], oper: Operator) extends Type[T] {
    override def toString() = x.toString() + oper.toString() + y.toString()
  }

  case class BinaryBooleanExpression[T](x: NumericType[T], y: NumericType[T], oper: BooleanOperator) extends Type[Boolean] {
    override def toString() = x.toString() + oper.toString() + y.toString()

    // TODO: This is a hack
    implicit def asBoolean(): Boolean = false
  }

  def +(that: Type[T]) = BinaryNumericExpression(this, com.yahoo.Scalops.toNumeric(that), Plus)

  def -(that: Type[T]) = BinaryNumericExpression(this, com.yahoo.Scalops.toNumeric(that), Minus)

  def *(that: Type[T]) = BinaryNumericExpression(this, com.yahoo.Scalops.toNumeric(that), Mult)

  def /(that: Type[T]) = BinaryNumericExpression(this, com.yahoo.Scalops.toNumeric(that), Div)

  def <(that: Type[T]) = BinaryBooleanExpression(this, com.yahoo.Scalops.toNumeric(that), LessThan)

  def ==(that: Type[T]) = BinaryBooleanExpression(this, com.yahoo.Scalops.toNumeric(that), Equals)

  def >(that: Type[T]) = BinaryBooleanExpression(this, com.yahoo.Scalops.toNumeric(that), MoreThan)
}

trait NumericTypeImplicits {
  implicit def toNumeric[T](t: Type[T])(implicit numeric: Numeric[T]) = new NumericType(t.value)

  implicit def boxNumeric[T](t: T)(implicit numeric: Numeric[T]) = new NumericType(Some(t))(numeric)

  implicit def unboxNumeric[T](t: NumericType[T]) = t.value.get

  implicit def boxDouble(d: Double) = NumericType[Double](Some(d))
}

object NumericType extends NumericTypeImplicits {

}

