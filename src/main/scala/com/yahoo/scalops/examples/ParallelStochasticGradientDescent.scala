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
 * An implementation of the Parallel Stochastic Gradient Descent for linear modeling presented in
 * "Parallelized Stochastic Gradient Descent" by Martin Zinkevich, Markus Weimer, Alex Smola, Lihong Li at NIPS 2010
 */

object ParallelStochasticGradientDescent {

  /**
   * A sequential SGD implementation that employs L2 regularization and accepts the loss function
   * (logistic regression, classification, ...) as a parameter.
   *
   *
   * Note that this class is written in pure scala and makes no use of scalops types etc.
   * Thus, it is an example of code written in pure scala (could even be java) that we can make use of in scalops.
   */
  class DIOSTrainer(lossFunction: (Vector, Example) => (Double, Double), eta: Double, lambda: Double) {

    def train(w: Vector, examples: Traversable[Example]): (Vector, Double) = {
      var result = w
      var lossSum: Double = 0.0
      for (x <- examples) {
        val (loss, gradient) = lossFunction(result, x)
        lossSum += loss
        result -= x.features * gradient * eta
        result *= (1.0 - lambda * eta)
      }
      return (result, lossSum)
    }
  }

  /**
   * The environment type for StochasticGradientDescent
   */
  case class SGDEnvironment(var w: VectorType, var objectiveFunctionValue: DoubleType) extends CompositeType

  def train(trainingExamples: Queryable[Example], trainer: DIOSTrainer, lambda: Double, nBins: Int = 100): Vector = {
    // Randomly group the data set into nBins
    val groups = trainingExamples.group(x => x.identifier.hashCode() % nBins)

    val initialValue = new SGDEnvironment(w = VectorType.zeros(1000), objectiveFunctionValue = Double.MaxValue)

    val result = loop(initialValue, (e: SGDEnvironment) => e.objectiveFunctionValue > 0.01) {
      env => {
        // Compute the models for each bin
        val models = groups.map {
          g => trainer.train(env.w, g._2)
        }
        // Average the models
        env.w = models.map(e => e._1).reduce(_ + _) / nBins
        // Compute the current loss function value for convergence
        env.objectiveFunctionValue = models.map(e => e._2).reduce(_ + _) / nBins + lambda * env.w.norm2()
        env
      }

    }
    result.w
  }
}