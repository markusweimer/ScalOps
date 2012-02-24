package com.yahoo.scalops.examples

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

trait Vector {

  def apply(index: Int): Double

  def update(index: Int, value: Double)

  def size(): Int

  def *(that: Double): Vector

  def +(that: Vector): Vector

  def -(that: Vector): Vector

  def -=(that: Vector) {
    require(this.size == that.size)
    for (i <- 0 until size) {
      this(i) -= that(i)
    }
  }

  def +=(that: Vector) {
    require(this.size == that.size)
    for (i <- 0 until size) {
      this(i) += that(i)
    }

  }

  /**
   * (that-this).norm2()
   */
  def distance2(that: Vector): Double = math.sqrt((for (i <- 0 until this.size) yield (math.pow(that(i) - this(i), 2.0))).reduce(_ + _))

  def unary_-(): Vector = this * (-1.0)

  def /(that: Double): Vector = this * (1.0 / that)

  def dot(that: Vector): Double = {
    var result = 0.0
    for (i <- 0 until this.size) {
      result += this(i) * that(i)
    }
    return result
  }

  def norm1(): Double = {
    var result = 0.0
    for (i <- 0 until this.size) {
      result += math.abs(this(i))
    }
    return result
  }

  def norm2(): Double = math.sqrt(this.dot(this))
}

