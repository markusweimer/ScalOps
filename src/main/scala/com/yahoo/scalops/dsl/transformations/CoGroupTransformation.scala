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

case class CoGroupTransformation[K, R](val input: JoinBranch[K, _]*) extends Transformation[R] {
  override def toString = input.mkString("COGROUP ", ", ", "")

  private[this] var parallelism: Option[scala.Int] = None

  def parallel(level: scala.Int) = {
    this.parallelism = Some(level)
    this
  }

}

trait CoGroupTransformationImplicits {

  def cogroup[K, B1, B2](b1: JoinBranch[K, B1], b2: JoinBranch[K, B2]) = CoGroupTransformation[K, (K, Traversable[B1], Traversable[B2])](b1, b2)

  def cogroup[K, B1, B2, B3](
                              b1: JoinBranch[K, B1],
                              b2: JoinBranch[K, B2],
                              b3: JoinBranch[K, B3]) = CoGroupTransformation[K, (K, Traversable[B1], Traversable[B2], Traversable[B3])](b1, b2, b3)

  def cogroup[K, B1, B2, B3, B4](
                                  b1: JoinBranch[K, B1],
                                  b2: JoinBranch[K, B2],
                                  b3: JoinBranch[K, B3],
                                  b4: JoinBranch[K, B4]) = CoGroupTransformation[K, (K, Traversable[B1], Traversable[B2], Traversable[B3], Traversable[B4])](b1, b2, b3, b4)

  def cogroup[K, B1, B2, B3, B4, B5](
                                      b1: JoinBranch[K, B1],
                                      b2: JoinBranch[K, B2],
                                      b3: JoinBranch[K, B3],
                                      b4: JoinBranch[K, B4],
                                      b5: JoinBranch[K, B5]) = CoGroupTransformation[K, (K, Traversable[B1], Traversable[B2], Traversable[B3], Traversable[B4], Traversable[B5])](b1, b2, b3, b4, b5)

  def cogroup[K, B1, B2, B3, B4, B5, B6](
                                          b1: JoinBranch[K, B1],
                                          b2: JoinBranch[K, B2],
                                          b3: JoinBranch[K, B3],
                                          b4: JoinBranch[K, B4],
                                          b5: JoinBranch[K, B5],
                                          b6: JoinBranch[K, B6]) = CoGroupTransformation[K, (K, Traversable[B1], Traversable[B2], Traversable[B3], Traversable[B4], Traversable[B5], Traversable[B6])](b1, b2, b3, b4, b5, b6)

  def cogroup[K, B1, B2, B3, B4, B5, B6, B7](
                                              b1: JoinBranch[K, B1],
                                              b2: JoinBranch[K, B2],
                                              b3: JoinBranch[K, B3],
                                              b4: JoinBranch[K, B4],
                                              b5: JoinBranch[K, B5],
                                              b6: JoinBranch[K, B6],
                                              b7: JoinBranch[K, B7]) = CoGroupTransformation[K, (K, Traversable[B1], Traversable[B2], Traversable[B3], Traversable[B4], Traversable[B5], Traversable[B6], Traversable[B7])](b1, b2, b3, b4, b5, b6, b7)

}