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
 * An example implementation of KMeans
 *
 */
object KMeans {

  /**
   * A cluster centroid
   */
  case class Centroid(v: Vector, id: Int)

  /**
   * Finds the closest of the centroids in the given list to the given vector
   */
  def computeClosestCentroid(v: Vector, centroids: Traversable[Centroid]): Centroid = {
    centroids.map(x => (x, v.distance2(x.v))).minBy(_._2)._1
  }

  /**
   * Creates a set of random centroids for initialization
   */
  def makeInitialCentroids(k: Int, d: Int) = {
    (for {i <- 0 until k} yield new Centroid(DenseVector.random(d), i)).toList
  }

  def mean(vectors: Traversable[Vector]): Vector = {
    vectors.reduce(_ + _) / vectors.size
  }

  def makeCentroid(id: Int, vectors: Traversable[Vector]) = new Centroid(mean(vectors), id)

  def cluster(data: Queryable[Vector], k: Int, d: Int) = {
    val initialValue: ListType[Centroid] = makeInitialCentroids(k, d)

    val result = loop(initialValue, 0 until 10) {
      centroids => {
        // Assign all vectors to their closest centroid and group by that centroid
        val groups = data.map(x => (x, computeClosestCentroid(x, centroids).id)).group(_._2)
        // Compute new centroids
        val newcentroids = groups.map {
          g => makeCentroid(g._1, g._2.map(_._1))
        }
        // Assign to environment
        newcentroids.collect()
      }
    }
    result
  }

  def main(args: Array[String]) {
    val data = load[Vector]("hdfs://example/")
    println(cluster(data, 2, 2))
  }
}