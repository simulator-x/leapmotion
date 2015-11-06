package simx.components.io.leapmotion

import simplex3d.math.float._
import simplex3d.math.floatx.ConstVec3f
import simx.core.entity.description.{EntityAspect, SValSet}
import simx.core.helper.chirality._
import simx.core.ontology.{types, EntityDescription, GroundedSymbol, Symbols}

/**
 * Created by chrisz on 03/06/15.
 */
object Fingers {

  val left: Set[Finger] = Set(
    new Finger(Symbols.ringFinger, Left),
    new Finger(Symbols.middleFinger, Left),
    new Finger(Symbols.thumb, Left),
    new Finger(Symbols.indexFinger, Left),
    new Finger(Symbols.littleFinger, Left)
    )

  val right: Set[Finger] = Set(
    new Finger(Symbols.ringFinger, Right),
    new Finger(Symbols.middleFinger, Right),
    new Finger(Symbols.thumb, Right),
    new Finger(Symbols.indexFinger, Right),
    new Finger(Symbols.littleFinger, Right)
  )
}

class Finger(fingerType: GroundedSymbol, chirality: Chirality) {



  def toDesc(): EntityDescription = new EntityDescription(
    FingerAspect(isLeft) :: Nil,
    simpleName(),
    additionalProperties = SValSet(types.EntityType.asConst(fingerType), types.Chirality.asConst(chirality))
  )

  private def simpleName() = {
    val fingerName = fingerType.toSymbol.name
    Symbol("[" + chirality.toString + "] " + Character.toUpperCase(fingerName.charAt(0)) + fingerName.substring(1))
  }

  private def isLeft: Boolean = {
    if (chirality.equals(Left)) true else false
  }

}

case class FingerAspect(isLeft: Boolean) extends EntityAspect(Symbols.leapmotion, Symbols.finger) {

  /**
   * the features the entity will at least have when it is created
   * @return the features the entity will at least have when it is created
   */
  def getFeatures = Set(types.Direction, types.Position, types.Velocity)

  /**
   * the features the component for which this aspect is designed *must* provide (i.e. provide initial values for those
   * features)
   * @return a set of features
   */
  def getProvidings = Set(types.Direction, types.Position, types.Velocity)

  /**
   *
   * The list of create parameters
   * @return a list of [[simx.core.entity.description.SVal]]'s containing the information needed to instanciate an
   *         entity with this aspect
   */
  def getCreateParams =
    addCVars(SValSet(
      types.Identifier(if(isLeft) Symbol(Left.toString) else Symbol(Right.toString)),
      types.Direction(ConstVec3f(0,0,0)),
      types.Position(ConstVec3(0,0,0)),
      types.Velocity(ConstVec3(0,0,0))
    ))
}
