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

import com.yahoo.scalops.examples.MLTypes.Example

/**
 * Gradient computations for a bunch of loss functions
 * @author Markus Weimer <weimer@yahoo-inc.com>
 */
object GradientFunctions {
  def computeSquaredErrorLossAndGradient(ex: Example, w: Vector): (Double, Vector) = computeSquaredErrorLossAndGradient(ex.label, ex.features, w)

  def computeSquaredErrorLossAndGradient(y: Double, x: Vector, w: Vector): (Double, Vector) = {
    val diff = (w.dot(x) - y)
    return (diff * diff, x * 2.0 * diff)
  }

}