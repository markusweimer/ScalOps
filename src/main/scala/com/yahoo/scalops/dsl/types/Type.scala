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

/**
 * The base type for all scalops Types
 *
 * The type subclasses all consist of three things:
 *
 * - The Type class itself
 * - A Trait with implicit conversions. This trait is woven into in Scalops
 * - A companion object that extends the trait with the implicit conversions and maybe adds additional methods
 */
trait Type[T] {
  def value: Option[T] = None
}

trait TypeImplicits {

  private case class TypeContainer[T](override val value: Option[T]) extends Type[T]

  implicit def box[T](x: T): Type[T] = new TypeContainer[T](Some(x))

  implicit def unbox[T](x: Type[T]) = x.value
}

object Type extends TypeImplicits