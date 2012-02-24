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

package com.yahoo.scalops.examples

import com.yahoo.Scalops._

/**
 * An sample scalops-implementation of
 *
 * "Large-Scale Matrix Factorization with Distributed Stochastic Gradient Descent"
 *
 * presented by Rainer Gemulla, Peter Haas, Erik Nijkamp and Yannis Sismanis at KDD 2011
 *
 * It computes F:= U*V such that (F-Y)^2 is minimized s.t. regularization
 *
 * @author Markus Weimer <weimer@yahoo-inc.com>
 */
object MatrixFactorization {

  /**
   * A factor vector U_i
   */
  case class RowVector(i: Int, v: Vector)

  /**
   * A factor vector V_j
   */
  case class ColVector(j: Int, v: Vector)

  /**
   * A entry in the data matrix Y
   */
  case class MatrixEntry(i: Int, j: Int, v: Double)

  /**
   * A naive partitioner.
   */
  class Partitioner(nBlocks: Int, maxRow: Int, maxCol: Int) {
    lazy val rowBlockSize = maxRow / nBlocks
    lazy val colBlockSize = maxCol / nBlocks

    private def getRowPartition(i: Int, iteration: Int) = (i % rowBlockSize) + iteration

    def apply(u: RowVector, iteration: Int) = getRowPartition(u.i, iteration)

    private def getColPartition(j: Int, iteration: Int) = (j % colBlockSize) + iteration

    def apply(v: ColVector, iteration: Int) = getColPartition(v.j, iteration)

    def apply(y: MatrixEntry, iteration: Int): Int = {
      val rowBlock = getRowPartition(y.i, iteration)
      val colBlock = getColPartition(y.j, iteration)
      assert(rowBlock == colBlock)
      return rowBlock
    }
  }

  /**
   * A basic least squares matrix factorization update step with L2 regularization using SGD
   *
   * @param Y the input data
   * @param U the set of row vectorts to update over
   * @param V the set of column vectors
   * @param eta the learning rate
   * @param lambda the regularization constant
   *
   * @return new row and column vectors after one SGD pass over the data
   *
   */
  def update(Y: Traversable[MatrixEntry], U: Traversable[RowVector], V: Traversable[ColVector], eta: Double, lambda: Double): (Traversable[RowVector], Traversable[ColVector]) = {
    // Index the vectors contained in U and V in a map from their i, j to the vector
    val uMap = Map(U.map(x => x.i -> x.v).toList: _*)
    val vMap = Map(V.map(x => x.j -> x.v).toList: _*)

    // Update the model
    for (y <- Y) {
      val u = uMap(y.i)
      val v = vMap(y.j)
      val gradient = 2.0 * eta * (u.dot(v) - y.v)
      uMap(y.i) -= v * gradient
      vMap(y.j) -= u * gradient
    }

    // Regularize the model
    uMap.values.map(_ * (1.0 - eta * lambda))
    vMap.values.map(_ * (1.0 - eta * lambda))

    // Emit the final model
    val u = uMap.map(e => new RowVector(e._1, e._2))
    val v = vMap.map(e => new ColVector(e._1, e._2))
    return (u, v)
  }

  /**
   * Trains a Matrixfactorization model.
   *
   * @param Y the known training data
   * @param nPartitions The number of partitions the training should happen in
   * @param d The dimensionality of the factor model learned
   * @param eta The learning rate to employ
   * @param lbda  The regularization constant
   * @param iter The number of iterations to run
   */
  def train(Y: Queryable[MatrixEntry], nPartitions: Int, d: Int, eta: Double, lbda: Double, nIter: Int): (Queryable[RowVector], Queryable[ColVector]) = {

    // Initialize the system ; WRONG: Make them unique
    val U = Y.map(y => new RowVector(y.i, DenseVector.random(d))).toTable
    val V = Y.map(y => new ColVector(y.j, DenseVector.random(d))).toTable

    // Compute some needed stats
    val maxRow: IntType = Y.map(y => y.i).reduce(scala.math.max(_, _))
    val maxCol: IntType = Y.map(y => y.j).reduce(scala.math.max(_, _))
    val part = new Partitioner(nPartitions, maxRow, maxCol)

    val initialValue: IntType = 0
    val result = loop(initialValue, (iter: IntType) => iter < nIter) {
      iter => {
        val groups = cogroup(Y by (part(_, iter)), U by (part(_, iter)), V by (part(_, iter)))
        val updatedModel = groups.map(g => update(Y = g._2, U = g._3, V = g._4, eta = eta, lambda = lbda))
        // Assign the new models to U and V
        U := updatedModel.map(x => flatten(x._1))
        V := updatedModel.map(x => flatten(x._2))
        iter + 1
      }
    }

    return (U, V)

  }

}