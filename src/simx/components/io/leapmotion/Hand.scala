package simx.components.io.leapmotion

import simplex3d.math.float._
import simx.core.entity.description.{SValSet, EntityAspect}
import simx.core.ontology.{types, Symbols, EntityDescription}
import simx.core.helper.chirality._
/**
 * Created by chrisz on 03/06/15.
 */
class Hand(isLeft: Boolean) {


  def toDesc(): EntityDescription = new EntityDescription(
    HandAspect(isLeft) :: fingers(),
    simpleName(),
    additionalProperties = SValSet(types.EntityType.asConst(Symbols.hand), types.Chirality.asConst(chirality))
  )

  private def fingers() = {
    if(isLeft) Fingers.left.map(_.toDesc()).toList else Fingers.right.map(_.toDesc()).toList
  }

  private def chirality: Chirality = {
    if(isLeft) Left else Right
  }

  private def simpleName(): Symbol ={
    Symbol("[" + chirality.toString + "] " + "Hand")
  }
}


case class HandAspect(isLeft: Boolean) extends EntityAspect(Symbols.leapmotion, Symbols.hand) {

  /**
   * the features the entity will at least have when it is created
   * @return the features the entity will at least have when it is created
   */
  def getFeatures = Set(types.Normal, types.Direction, types.Position, types.Velocity)

  /**
   * the features the component for which this aspect is designed *must* provide (i.e. provide initial values for those
   * features)
   * @return a set of features
   */
  def getProvidings = Set(types.Normal, types.Direction, types.Position, types.Velocity)

  /**
   *
   * The list of create parameters
   * @return a list of [[simx.core.entity.description.SVal]]'s containing the information needed to instanciate an
   *         entity with this aspect
   */
  def getCreateParams =
    addCVars(SValSet(
      types.Identifier(if(isLeft) Symbol(Left.toString) else Symbol(Right.toString)),
      types.Position(ConstVec3(0,0,0)),
      types.Normal(ConstVec3(0,0,0)),
      types.Direction(ConstVec3(0,0,0)),
      types.Velocity(ConstVec3(0,0,0))
    ))
}