package com.yahoo.scalops.dsl.actions

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

import com.yahoo.scalops.dsl.Queryable
import com.yahoo.scalops.dsl.types.Type
import com.yahoo.scalops.dsl.types.BooleanType
import com.yahoo.scalops.dsl.types.ListType

trait Action[T] extends Queryable[T] with Type[T]

/**
 * Mixed into Scalops._
 */
trait ActionImplicits {

  implicit def actionToBoolean(t: Action[Boolean]): BooleanType = BooleanType.typeToBooleanType(t)

  implicit def actionToList[T](t: Action[List[T]]): ListType[T] = ListType.typeToListType[T](t)

}