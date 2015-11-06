package simx.components.io.leapmotion

import simx.components.io.leapmotion.Math._
import simx.core.component.Component
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.{NamedSValSet, SValSet, EntityAspect}
import simx.core.entity.Entity
import com.leapmotion.leap._
import simx.core.ontology.types.Chirality
import simx.core.ontology.{GroundedSymbol, Symbols, EntityDescription, types}
import simplex3d.math.float._
import simx.core.entity.component.{ComponentAspect, EntityCreationHandling}
import simx.core.components.io.IODeviceProvider
import simx.core.helper.chirality

import simx.core.svaractor.{TimedRingBuffer, SVarActor}
import simx.core.svaractor.TimedRingBuffer.At


class BodyPart(entityProperty: SValSet) {
  def toEP = toEntityProperties
  def toEntityProperties = entityProperty
}

object LeapMotionComponent {
  lazy val trackedParts: List[BodyPart] = hands ::: fingers

  private val availableFingers = List(Symbols.thumb, Symbols.ringFinger, Symbols.middleFinger, Symbols.indexFinger, Symbols.littleFinger)
  private val chiralities = List(chirality.Left, chirality.Right)

  private def hands: List[BodyPart] = {
      new BodyPart(SValSet(types.EntityType(Symbols.hand), types.Chirality(chirality.Left))) ::
      new BodyPart(SValSet(types.EntityType(Symbols.hand), types.Chirality(chirality.Right))) :: Nil
  }

  private def fingers: List[BodyPart] = {
    var res: List[BodyPart] = Nil
    chiralities.foreach{chir => {
      availableFingers.foreach{fing => res ::= finger(fing, chir)}}
    }
    res
  }



  private def finger(entityType: GroundedSymbol, chir: chirality.Chirality): BodyPart = {
    new BodyPart(SValSet(types.EntityType(entityType), types.Chirality(chir)))
  }

}



case class LeapMotionComponentAspect(name : Symbol, args : Any*)
  extends ComponentAspect[LeapMotionComponent](Symbols.leapmotion, name, args)
{
  def getComponentFeatures: Set[ConvertibleTrait[_]] = Set()
  def getCreateParams: NamedSValSet = NamedSValSet(aspectType)
}


/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 9/23/13
 * Time: 2:06 PM
 */
class LeapMotionComponent(componentName : Symbol) extends Component(componentName,Symbols.leapmotion)
  with EntityCreationHandling with IODeviceProvider
{

  val debugPrintln = false
  val polling = false
  protected def finalizeConfiguration(e: Entity) = {}

  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) =
    SValSet()

  private var controller: Option[Controller] = None

  /**
   * called when the actor is started
   */
  override protected def startUp() {
    val leapmotionComponent = self
    if(!polling) {
      SVarActor.createActor(new SVarActor {
        override def startUp() {
          val l = new Listener() {
            override def onConnect(p1: Controller) {p1.enableGesture(Gesture.Type.TYPE_SWIPE)}
            override def onFrame(p1: Controller) {leapmotionComponent ! p1.frame}
          }
          val c = new Controller()
          c.addListener(l)
          System.in.read()
          c.removeListener(l)
        }
      })
    } else {
      controller = Some(new Controller())
      //activates swipe recognition of leap motion
      controller.get.enableGesture(Gesture.Type.TYPE_SWIPE)
    }


    new EntityDescription(
      List[EntityAspect](new Hand(true).toDesc(), new Hand(false).toDesc()),
      'LeapUser,
      List[Symbol](),
      Set[simx.core.ontology.Annotation](),
      additionalProperties = SValSet(types.EntityType.asConst(Symbols.user))
    ).realize()



//    new EntityDescription(
//      name = "Left Hand",
//      aspects = HandAspect(isLeft = true), new EntityDescription("some finger", FinderAspect())
//    ).realize()
//
//    new EntityDescription(
//      HandAspect(isLeft = false),
//      NameIt("Right Hand")
//    ).realize()
  }

  /**
   * Called for each simulation step the component should execute. The freqency with which this method is called
   * depends on the used [[simx.core.component.ExecutionStrategy]].
   */
  protected def performSimulationStep() {
    if(polling) controller.collect{case c => handleFrame(c.frame)}
    simulationCompleted()
  }

  val deltas = new Array[Long](600)
  for(i <- 0 until deltas.size) deltas(i) = 0L
  var idx = 0
  var last = System.currentTimeMillis()

  addHandler[Frame](msg => {
    val now = System.currentTimeMillis()
    val delta = now -last
    last = now
    deltas(idx) = delta
    idx = (idx+1)%deltas.size
    if(debugPrintln && (idx==0)) {
      println("[info][LeapMotionComponent] mean frame dalta " +
        deltas.sum.toFloat / deltas.size.toFloat)
    }
    handleFrame(msg)
  })

  protected def handleFrame(frame : Frame) {

      /* if one hand is inside tracking area, it is treated as right hand
       more than on hand, rightmost is treated as right hand, leftmost -> left hand
      */
      val firstHand = frame.hands.get(0)
      val secondHand = frame.hands.get(1)

      var rHand = firstHand
      var lHand = secondHand

      if(firstHand.isLeft) {
        lHand = firstHand
        rHand = secondHand
      }  else {
        lHand = secondHand
        rHand = firstHand
      }

      val rhFingers = rHand.fingers()
      val lhFingers = lHand.fingers()
      types.Normal

      leftHand.foreach(e => {
        updateHands(lHand, e, frame)
      })

      rightHand.foreach(e => {
        updateHands(rHand, e, frame)
      })

     fingersLeft.foreach(entry => {
      updateFingers(lhFingers, entry, frame)
     })

    fingersRight.foreach(entry => {
      updateFingers(rhFingers, entry, frame)
    })



  }


  def updateHands(h: com.leapmotion.leap.Hand, e: Entity, frame: Frame) {
    val position = types.Position(h.palmPosition())
    e.set(position)
    val normal = types.Normal(h.palmNormal())
    e.set(normal, At(System.currentTimeMillis()), TimedRingBuffer.UnbufferedTimeStoring)(_ => {})
    val direction = types.Direction(h.direction())
    e.set(direction, At(System.currentTimeMillis()), TimedRingBuffer.UnbufferedTimeStoring)(_ => {})
    val velocity = types.Velocity(h.palmVelocity())
    e.set(velocity, At(System.currentTimeMillis()), TimedRingBuffer.UnbufferedTimeStoring)(_ => {})
  }

  def updateFingers(fingers: FingerList, entry: (GroundedSymbol, Entity), frame: Frame) {
    val finger = fingers.get(___fingerMapping(entry._1))
    val e = entry._2
    e.set(types.Direction(finger.direction()), At(System.currentTimeMillis()), TimedRingBuffer.UnbufferedTimeStoring)(_ => {})
    e.set(types.Velocity(finger.tipVelocity()), At(System.currentTimeMillis()), TimedRingBuffer.UnbufferedTimeStoring)(_ => {})
    e.set(types.Position(finger.tipPosition()), At(System.currentTimeMillis()), TimedRingBuffer.UnbufferedTimeStoring)(_ => {})
  }



  private var fingersLeft: Map[GroundedSymbol, Entity] = Map()
  private var fingersRight: Map[GroundedSymbol, Entity] = Map()
  private var leftHand: Option[Entity] = None
  private var rightHand: Option[Entity] = None


  /**
   * provideInitialValues has to be called within this method with the full set of initial values to be provided
   * @note the component should create its local representation within this mehtod
   * @param toProvide the convertibletraits for which values shall be provided
   * @param aspect the aspect providing the context for this method call
   * @param e the entity to be filled
   * @param given a set of create parameters that were already provided
   *
   */
  protected def requestInitialValues(
                                      toProvide: Set[ConvertibleTrait[_]],
                                      aspect: EntityAspect,
                                      e: Entity,
                                      given: SValSet) {
    provideInitialValues(e, aspect.getCreateParams.combineWithValues(toProvide)._1)
  }

  /**
   * used to integrate the entity into the simulation
   * @param e the entity to be integrated
   * @param aspect the aspect which the component has to process
   */
  protected def entityConfigComplete(e: Entity, aspect: EntityAspect) {
    //Ignore component entity for now
    if(aspect.aspectType == Symbols.component)
      return

    val handType = aspect.getCreateParams.firstSValFor(types.Identifier).value
    updateLocalRep(e, handType)
  }

  private def updateLocalRep(e: Entity, handType: Symbol) {
    if (handType == Symbol(Left.toString)) addToRep(e, isLeft = true)
    else if (handType == Symbol(Right.toString)) addToRep(e, isLeft = false)
    else  println("Unknown hand type: " + handType.name)
  }

  private def addToRep(e: Entity, isLeft: Boolean): Unit ={
    e.get(types.EntityType).head(
      (eType) => {
        if (eType == Symbols.hand){
          if(isLeft) leftHand = Some(e) else rightHand = Some(e)
        } else {
          if(isLeft) fingersLeft = fingersLeft.updated(eType, e)
          else fingersRight = fingersRight.updated(eType, e)
        }
      }
    )
  }










  /**
   * (re)configure the component
   * @param params the configuration params
   */
  protected def configure(params: SValSet) {}

  /**
   * method to be implemented by each component. Will be called when an entity has to be removed from the
   * internal representation.
   * @param e the Entity to be removed
   */
  protected def removeFromLocalRep(e: Entity) {}



  val fingerMapping: Map[Finger.Type, GroundedSymbol] = Map(
    Finger.Type.TYPE_INDEX -> Symbols.indexFinger,
    Finger.Type.TYPE_MIDDLE -> Symbols.middleFinger,
    Finger.Type.TYPE_PINKY -> Symbols.littleFinger,
    Finger.Type.TYPE_RING -> Symbols.ringFinger,
    Finger.Type.TYPE_THUMB -> Symbols.thumb
  )

  val _fingerMapping: Map[GroundedSymbol, Finger.Type] = Map(
    Symbols.indexFinger -> Finger.Type.TYPE_INDEX,
    Symbols.middleFinger -> Finger.Type.TYPE_MIDDLE ,
    Symbols.littleFinger -> Finger.Type.TYPE_PINKY,
    Symbols.ringFinger -> Finger.Type.TYPE_RING,
    Symbols.thumb -> Finger.Type.TYPE_THUMB
  )

  val __fingerMapping: Map[Int, GroundedSymbol] = Map(
    1 -> Symbols.indexFinger,
    2 -> Symbols.middleFinger,
    4 -> Symbols.littleFinger,
    3 -> Symbols.ringFinger,
    0 -> Symbols.thumb
  )

  val ___fingerMapping: Map[GroundedSymbol, Int] = Map(
    Symbols.indexFinger -> 1,
    Symbols.middleFinger -> 2 ,
    Symbols.littleFinger -> 4,
    Symbols.ringFinger -> 3,
    Symbols.thumb -> 0
  )
}

