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

package com.yahoo.scalops.dsl.transformations

import com.yahoo.scalops.dsl.Queryable

/**
 * Map transformation: Apply a given function to all records in the queryable, yielding a new queryable of the output type.
 */
case class MapTransformation[T1, T2](val input: Queryable[T1], val f: T1 => T2) extends Transformation[T2] {
  override def toString = input.toString + ".map(" + f.toString + ")"
}

trait MapTransformationImplicits {
}