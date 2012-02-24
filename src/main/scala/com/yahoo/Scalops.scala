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

package com.yahoo

import com.yahoo.scalops.dsl.types.NumericTypeImplicits
import com.yahoo.scalops.dsl.transformations.MapTransformationImplicits
import com.yahoo.scalops.dsl.transformations.FilterTransformationImplicits
import com.yahoo.scalops.dsl.transformations.LoadTransformationImplicits
import com.yahoo.scalops.dsl.transformations.JoinTransformationImplicits
import com.yahoo.scalops.dsl.transformations.OrderTransformationImplicits
import com.yahoo.scalops.dsl.transformations.CoGroupTransformationImplicits
import com.yahoo.scalops.dsl.types.NumericTypeArithmetics
import com.yahoo.scalops.dsl.actions.ActionImplicits
import com.yahoo.scalops.dsl.types.BooleanImplicits
import com.yahoo.scalops.dsl.types.VectorImplicits
import com.yahoo.scalops.dsl.types.ListTypeImplicits
import com.yahoo.scalops.dsl.types.StringTypeImplicits
import com.yahoo.scalops.dsl.transformations.EmptyTableTransformationImplicits
import com.yahoo.scalops.dsl.oper.ScaloopImplicits

object Scalops extends NumericTypeImplicits
with NumericTypeArithmetics
with BooleanImplicits
with VectorImplicits
with ListTypeImplicits
with StringTypeImplicits
with ActionImplicits
with EmptyTableTransformationImplicits
with LoadTransformationImplicits
with MapTransformationImplicits
with FilterTransformationImplicits
with OrderTransformationImplicits
with ScaloopImplicits
with JoinTransformationImplicits
with CoGroupTransformationImplicits {

  // Types
  type Queryable[T] = com.yahoo.scalops.dsl.Queryable[T]
  type Type[T] = com.yahoo.scalops.dsl.types.Type[T]
  type NumericType[T] = com.yahoo.scalops.dsl.types.NumericType[T]
  type DoubleType = NumericType[Double]
  type FloatType = NumericType[Float]
  type IntType = NumericType[Int]
  type LongType = NumericType[Long]
  type StringType = com.yahoo.scalops.dsl.types.StringType
  type VectorType = com.yahoo.scalops.dsl.types.VectorType
  type ListType[T] = com.yahoo.scalops.dsl.types.ListType[T]
  type CompositeType = com.yahoo.scalops.dsl.types.CompositeType
  type BooleanType = com.yahoo.scalops.dsl.types.BooleanType

  // Flatten
  case class FunctionFlatten[T](t: Traversable[T]) extends Throwable

  def flatten[T](t: Traversable[T]): T = throw new FunctionFlatten(t)
}