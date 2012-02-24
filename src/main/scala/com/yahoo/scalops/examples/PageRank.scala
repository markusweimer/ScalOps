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
 * Compare with: http://ofps.oreilly.com/titles/9781449302641/embedding.html
 */
class PageRank {

  class Page(val url: String, val rank: Double, val links: List[String])

  def run(delta: Float) = {
    var pages = load[Page]("hdfs://example")

    val initialValue: DoubleType = Double.MaxValue

    // Loop until the objectiveFunctionvalue is below 0.01
    val result = loop(initialValue, (delta: DoubleType) => delta < 0.01) {
      delta: DoubleType => {
        var outbound_pagerank = pages.map(page => (page.rank / page.links.size, flatten(page.links)))
        var cogrp = cogroup(outbound_pagerank by (_._2), pages by (_.url))

        var new_pagerank = cogrp.map(row => (new Page(row._1, (1 - delta) + row._2.map(_._1).reduce(_ + _) * delta, row._3.head.links), row._3.head.rank))
        pages := new_pagerank.map(_._1)
        var diff = new_pagerank.map(page => math.abs(page._2 - page._1.rank))
        diff.reduce(math.max(_, _))
      }
    }
  }
}