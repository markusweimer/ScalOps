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
import com.yahoo.scalops.dsl.types.VectorType

import com.yahoo.scalops.examples.MLTypes.Example

/**
 * A example batch gradient descent implementation for scalops
 *
 * @author Markus Weimer <weimer@yahoo-inc.com>
 */
object BatchGradientDescent {

  /**
   * Train a linear model using basic batch gradient descent
   *
   * The loss function to be optimized (e.g. for logistic regression, least squares, linear SVM, ...) is passed as a
   * parameter to this train function in the form of computeGradient, which can compute the gradient of the loss with
   * respect to w for a single example.
   *
   * @param trainingExamples The set of examples to train the system on
   * @param computeGradient A function that computes the loss and gradient wrt. the weight vector
   * @param eta the learning rate to apply
   * @param lambda the regularization constant
   * @param eps the minimum change between iterations to keep the algorithm running
   */
  def train(trainingExamples: Queryable[Example], computeGradient: (Example, Vector) => (Double, Vector), eta: Double, lambda: Double, eps: Double = 0.01): Vector = {

    /**
     * The Environment of the batch gradient descent loop: A weight vector and a sum of all losses.
     */
    case class LoopEnvironment(var w: VectorType, var objectiveFunctionValue: DoubleType, var eta: DoubleType) extends CompositeType

    // Set the inital value for the environment: A empty vector and maximum loss
    val initialValue = new LoopEnvironment(w = VectorType.zeros(1000), objectiveFunctionValue = Double.MaxValue, eta = eta)

    // Loop until the objectiveFunctionvalue is below 0.01
    val result = loop(initialValue, (env: LoopEnvironment) => (env.objectiveFunctionValue > eps)) {
      env: LoopEnvironment => {
        // Compute the loss and gradient in parallel for all data points
        // computeGradient returns a tuple(Double, Vector) for each data point
        val lossesAndGradients = trainingExamples.map(x => computeGradient(x, env.w))

        // Sum up the loss and regularizer to update the objective function value
        env.objectiveFunctionValue = lossesAndGradients.map(x => x._1).reduce(_ + _)
        env.objectiveFunctionValue += lambda * env.w.get.norm2

        // Sum up the gradients and update the weight vector
        // Note that there are of course more clever ways to do this (e.g. a line search along the gradient for the best step size)
        // However, we chose to use a simple step here for simplicity of the example
        env.w -= lossesAndGradients.map(x => x._2).reduce(_ + _) * eta

        // Apply the l2 regularizer
        env.w *= (1.0 - env.eta * lambda)

        // Update the learning rate. There are many ways to do this. Here for the sake of brevity: Just shrink it.
        env.eta *= 0.9

        // The loop body has to return the new environment
        env
      }
    }
    println(result)
    return result.w
  }

  def bgd(Y: Queryable[Example], g: (Example, Vector) => Vector, e: Double, l: Double) =
    loop(VectorType.zeros(1000), 0 until 100) {
      w => (w - (Y.map(x => g(x, w)).reduce(_ + _) * e)) * (1.0 - e * l)
    }

  def main(args: Array[String]) {
    val trainingData = load[Example]("hdfs://example/")
    val w = train(trainingData, GradientFunctions.computeSquaredErrorLossAndGradient, eta = 0.01, lambda = 0.1)
    println(w)
  }
}