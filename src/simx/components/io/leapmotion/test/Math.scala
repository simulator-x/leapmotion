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
 * HCI Group at the University of Wuerzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.io.leapmotion.test

import com.leapmotion.leap.Matrix
import simplex3d.math.float._
import simx.components.io.leapmotion.Math._

/**
 * Created by chrisz on 08/06/15.
 */
object Math {

  private val angles = List(
    (math.Pi/2.0).toFloat,
    math.Pi.toFloat,
    2f,
    10f,
    -15f
  )

  private val vectors = List(
    Vec3(1,0,0),
    simplex3d.math.float.functions.normalize(Vec3(1,2,3))
  )

  def main(args: Array[String]) {
    for( v <- vectors; a <- angles) testConversion(v,a)
  }

  private def testConversion(v: Vec3, angle: Float): Unit ={
    //Leap
    val axis = new com.leapmotion.leap.Vector(v.x,v.y,v.z)
    val leapMat = new Matrix()
    leapMat.setRotation(axis, angle)

    //Simplex3D
    val simplexMat = ConstMat4(simplex3d.math.float.functions.rotationMat(angle, v))



    val converted: ConstMat4 = leapMat

    println("------")
    println(converted)
    println(simplexMat)
  }





}
