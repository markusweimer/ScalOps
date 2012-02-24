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

object Test {

  class Book(val title: String, val author: String, val address: String) {
    override def toString = "Book: " + title + ", " + author + ", " + address
  }

  class Author(val name: String, val address: String, val books: List[Book], val years: Double) {
    override val toString = "Author: " + name + ", " + address + ", " + books + ", " + years
  }

  def run = {

    val authors = load[Author]("hdfs://examples/authors.dat")

    val books = load[Book]("hdfs://examples/book.dat")

    val t1 = filter(books) by (_.author == "F")

    val r1 = join(books by(_.author, _.address), authors by(_.name, _.address)).
      filter(x => x._1.address == "Foobar" && x._2.name != "Baz").
      group(_._2.name + "foobar").map(x => (x._1, x._2.size, flatten(x._2), x._2.map(_._2.years).reduce(_ + _)))

    println("R1: " + r1.toString)

    val r2 = filter(r1) by (_._2 < 100)
    println("R2: " + r2.toString)

    val r3 = books.map {
      b =>
        val x = "foobar"
        if (x == "foo") null else new Book(x, b.address, b.author)
    }
    println("R3: " + r3)

    val r4 = filter(r3) by (_.address == "foobar")
    println("R4: " + r4)
  }
}

object App {

  def main(args: Array[String]) {
    println("START TEST")
    Test.run
  }

}
