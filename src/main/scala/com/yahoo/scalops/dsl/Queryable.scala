package com.yahoo.scalops.dsl

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

import actions.{IsEmptyAction, ReduceAction, CollectAction, CountAction}
import com.yahoo.scalops.dsl.transformations.MapTransformation
import com.yahoo.scalops.dsl.transformations.FilterTransformation
import com.yahoo.scalops.dsl.transformations.UpdateTransformation
import com.yahoo.scalops.dsl.transformations.GroupTransformation
import com.yahoo.scalops.dsl.transformations.OrderTransformation
import com.yahoo.scalops.dsl.transformations.TableTransformation
import com.yahoo.scalops.dsl.types.Type
import com.yahoo.scalops.dsl.types.ListType

trait Queryable[T] {

  // Transformations
  /**
   * Materializes the Query into a table
   */
  def toTable = new TableTransformation(this)

  /**
   * Applies the given function to each element of the Queryable, resulting in a new Queryable of the results
   */
  def map[T2](f: T => T2): Queryable[T2] = new MapTransformation(this, f)

  /**
   * Filter the Queryable by the given predicate. Those elements where predicate evaluates to true will be preserved
   */
  def filter(predicate: T => Boolean): Queryable[T] = new FilterTransformation(this, predicate)

  /**
   * Overwrite the queryable with the elements found in the other queryable
   */
  def :=(that: Queryable[T]): Queryable[T] = new UpdateTransformation[T](this, that)

  /**
   * Group the records in the Queryable using the given grouping predicate
   */
  def group[K](predicate: T => K): Queryable[(K, Traversable[T])] = new GroupTransformation[K, T](this, predicate)

  def sort[R <% Ordered[R]](predicate: T => R): Queryable[T] = new OrderTransformation[T, R](this, predicate)

  // Actions

  /**
   * Count the number of elements in the Queryable
   */
  def count() = new CountAction(this)

  def size = count()

  /**
   * Collect the elements of the Queryable into a local collection.
   */
  def collect(): ListType[T] = new CollectAction[T](this)

  /**
   * Apply the given function to reduce the Queryable to a scalar
   */
  def reduce(f: (T, T) => T): Type[T] = new ReduceAction(this, f)

  /**
   * Determine whether the Queryable is empty.
   */
  def isEmpty = new IsEmptyAction(this)

}