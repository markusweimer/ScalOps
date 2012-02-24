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

package com.yahoo.scalops.dsl.types

class ListType[T] extends Type[List[T]]

trait ListTypeImplicits {

  private[ListTypeImplicits] case class ListContainer[T](override val value: Option[List[T]]) extends ListType[T]

  implicit def listToListType[T](l: List[T]): ListType[T] = new ListContainer[T](Some(l))

  implicit def listTypeToList[T](lt: ListType[T]): Traversable[T] = lt.value.get

  implicit def typeToListType[T](t: Type[List[T]]): ListType[T] = ListContainer[T](t.value)

}

object ListType extends ListTypeImplicits