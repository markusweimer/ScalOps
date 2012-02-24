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

object Pregel {

  case class Node(id: Int, neighbors: List[Int])

  case class Message(to: Int)

  /**
   * An implementation of pregel in scalops
   *
   * @param nodes The set of nodes in the graph
   * @param f A pregel-style update function
   *
   */
  def run(nodes: Queryable[Node], f: (Node, Traversable[Message]) => (Node, Traversable[Message])) {

    def nf(n: Node, m: Traversable[Message]) = if (m == null) (n, m) else f(n, m)

    val messages = table[Message]()
    val init: BooleanType = false

    loop(init, (b: BooleanType) => !b) {
      b => {
        val x = cogroup((nodes by (_.id)).outer, messages by (_.to))
        val msgAndNodes = x.map(e => nf(e._2.head, e._3))
        messages := msgAndNodes.map(x => flatten(x._2))
        nodes := msgAndNodes.map(_._1)
        messages.isEmpty
      }
    }
  }

}