/*
 * Copyright 2015 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.io.leapmotion

import com.leapmotion.leap.{Matrix, Vector}
import simplex3d.math.float._

/**
 * Created by chrisz on 08/06/15.
 */
object Math {
  implicit def toConstMat4(m : Matrix): ConstMat4 = {
    val d = m.toArray4x4
    ConstMat4(d(0), d(4), d(8), d(3), d(1), d(5), d(9), d(7), d(2), d(6), d(10), d(11), d(12), d(13), d(14), d(15))
  }

  implicit def toConstVec3(v: Vector): ConstVec3 =
    ConstVec3(v.getX, v.getY, v.getZ)
}
