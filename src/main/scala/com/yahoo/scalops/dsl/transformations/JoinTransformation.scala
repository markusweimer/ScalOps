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

case class JoinTransformation[K, R](val input: JoinBranch[K, _]*) extends Transformation[R] {

  object JoinMethod extends Enumeration {
    val Replicated, Skewed, Merge = Value
  }

  type Method = JoinMethod.Value

  override def toString = input.mkString("join(", ", ", ")")

  private[this] var method: Option[Method] = None

  private[this] var parallelism: Option[scala.Int] = None

  def using(m: Method) {
    this.method = Some(m)
    this
  }

  def parallel(level: scala.Int) = {
    this.parallelism = Some(level)
    this
  }
}

trait JoinTransformationImplicits {
  implicit def queryableToBranch[T](input: Queryable[T]) = JoinBranchImplicit[T](input)

  implicit def transformationToBranch[T](input: Transformation[T]) = JoinBranchImplicit[T](input)

  def join[K, B1, B2](b1: JoinBranch[K, B1], b2: JoinBranch[K, B2]) = JoinTransformation[K, (B1, B2)](b1, b2)

  def join[K, B1, B2, B3](
                           b1: JoinBranch[K, B1],
                           b2: JoinBranch[K, B2],
                           b3: JoinBranch[K, B3]) = JoinTransformation[K, (B1, B2, B3)](b1, b2, b3)

  def join[K, B1, B2, B3, B4](
                               b1: JoinBranch[K, B1],
                               b2: JoinBranch[K, B2],
                               b3: JoinBranch[K, B3],
                               b4: JoinBranch[K, B4]) = JoinTransformation[K, (B1, B2, B3, B4)](b1, b2, b3, b4)

  def join[K, B1, B2, B3, B4, B5](
                                   b1: JoinBranch[K, B1],
                                   b2: JoinBranch[K, B2],
                                   b3: JoinBranch[K, B3],
                                   b4: JoinBranch[K, B4],
                                   b5: JoinBranch[K, B5]) = JoinTransformation[K, (B1, B2, B3, B4, B5)](b1, b2, b3, b4, b5)

  def join[K, B1, B2, B3, B4, B5, B6](
                                       b1: JoinBranch[K, B1],
                                       b2: JoinBranch[K, B2],
                                       b3: JoinBranch[K, B3],
                                       b4: JoinBranch[K, B4],
                                       b5: JoinBranch[K, B5],
                                       b6: JoinBranch[K, B6]) = JoinTransformation[K, (B1, B2, B3, B4, B5, B6)](b1, b2, b3, b4, b5, b6)

  def join[K, B1, B2, B3, B4, B5, B6, B7](
                                           b1: JoinBranch[K, B1],
                                           b2: JoinBranch[K, B2],
                                           b3: JoinBranch[K, B3],
                                           b4: JoinBranch[K, B4],
                                           b5: JoinBranch[K, B5],
                                           b6: JoinBranch[K, B6],
                                           b7: JoinBranch[K, B7]) = JoinTransformation[K, (B1, B2, B3, B4, B5, B6, B7)](b1, b2, b3, b4, b5, b6, b7)
}