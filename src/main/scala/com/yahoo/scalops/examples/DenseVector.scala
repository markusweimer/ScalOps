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

class DenseVector(val a: Array[Double]) extends Vector {
  def apply(index: Int) = a(index)

  def update(index: Int, value: Double) = {
    a(index) = value
  }

  def size() = a.size

  def *(that: Double): DenseVector = new DenseVector(a.map(x => x * that))


  def +(that: Vector): DenseVector = {
    val result = new Array[Double](size)
    for (i <- 0 until size) {
      result(i) = this(i) + that(i)
    }
    return new DenseVector(result)
  }

  def -(that: Vector): DenseVector = {
    val result = new Array[Double](size)
    for (i <- 0 until size) {
      result(i) = this(i) - that(i)
    }
    return new DenseVector(result)
  }
}


object DenseVector {

  def apply(v: Vector): DenseVector = {
    val result = new Array[Double](v.size)
    for (i <- 0 until v.size) {
      result(i) = v(i)
    }
    new DenseVector(result)
  }

  def apply(a: Array[Double]): DenseVector = new DenseVector(a)


  /**
   * Creates a zero vector of the given size
   */
  def zeros(size: Int): DenseVector = new DenseVector(new Array[Double](size))

  /**
   * Creates a random vector of the given size
   */
  def random(size: Int): DenseVector = {
    val a = new Array[Double](size)
    for (i <- 0 until a.length) {
      a(i) = math.random
    }

    DenseVector(a)
  }

  def apply(x: Double, xs: Double*): DenseVector = {
    val array = Array[Double](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) {
      array(i) = x
      i += 1
    }
    DenseVector(array)
  }
}