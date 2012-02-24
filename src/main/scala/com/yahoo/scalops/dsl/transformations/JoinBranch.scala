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

case class JoinBranch[K, T](val input: Queryable[T], val key: T => K) {
  private[this] var _outer: Boolean = false

  override def toString = input.toString() + ".by(" + key.toString() + ")"

  def outer = {
    _outer = true;
    this
  }
}

case class JoinBranchImplicit[T](val input: Queryable[T]) {
  def by[K1](k1: T => K1) = new JoinBranch[K1, T](input, k1)

  def by[K1, K2](k1: T => K1, k2: T => K2) =
    new JoinBranch[Tuple2[K1, K2], T](input, (t: T) => new Tuple2(k1(t), k2(t)))

  def by[K1, K2, K3](k1: T => K1, k2: T => K2, k3: T => K3) =
    new JoinBranch[Tuple3[K1, K2, K3], T](input, (t: T) => new Tuple3(k1(t), k2(t), k3(t)))

  def by[K1, K2, K3, K4](k1: T => K1, k2: T => K2, k3: T => K3, k4: T => K4) =
    new JoinBranch[Tuple4[K1, K2, K3, K4], T](input, (t: T) => new Tuple4(k1(t), k2(t), k3(t), k4(t)))

  def by[K1, K2, K3, K4, K5](k1: T => K1, k2: T => K2, k3: T => K3, k4: T => K4, k5: T => K5) =
    new JoinBranch[Tuple5[K1, K2, K3, K4, K5], T](input, (t: T) => new Tuple5(k1(t), k2(t), k3(t), k4(t), k5(t)))

  def by[K1, K2, K3, K4, K5, K6](k1: T => K1, k2: T => K2, k3: T => K3, k4: T => K4, k5: T => K5, k6: T => K6) =
    new JoinBranch[Tuple6[K1, K2, K3, K4, K5, K6], T](input, (t: T) => new Tuple6(k1(t), k2(t), k3(t), k4(t), k5(t), k6(t)))
}