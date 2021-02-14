package logics

import gui.MainUI._
import gui.SimMap
import java.util._
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.collection.immutable.Vector

/*
 * All the logics behind the boids. The bulk of the program is here.
 *
 * The boids are divided into 2 classes: LeadingBoid and FollowingBoid,
 * with both inherit the Boid abstract class.
 *
 * All the boids are modeled after Craig Reynolds's research:
 * https://www.red3d.com/cwr/boids/
 *
 */

abstract class Boid(val simulation: Simulation, var position: MathVec, var velocity: MathVec) {

  val simMapWidth = simulation.width
  val simMapHeight = simulation.height

  /*
 	* How the boids move: adding steering force to the velocity vector.
 	*/
  def speedLimit: Double
  def steering: MathVec
  def move: Unit = {
    var realSteering = steering.truncate(simulation.forceConstance)
    velocity = (velocity + realSteering).truncate(speedLimit)
    position = position + velocity
    position = position.bound(simMapWidth, 12, simMapHeight, 16)
  }

  /*
   * How all boids avoid LeadingBoids. If they are too close, they will flee from
   * the position of the LeadingBoid. If they are on its way - achieved by creating
   * a single point in front of the boid - they will flee from it.
   */
  def avoidLeader: MathVec = {
    var total = new MathVec(0, 0)
    val slowRadius = 10

    def isOnLeaderSight(leader: LeadingBoid, spaceInFront: MathVec): Boolean = {
      this.getPosition.distanceTo(spaceInFront) <= 50
    }

    def tooClose(leader: LeadingBoid): Boolean =
      this.rawDistance(leader) <= simulation.distanceToLeader

    for (i <- simulation.allLeadingBoids) {
      if (i != this) {
        if (tooClose(i)) total = total + this.flee(i.getPosition) *
          (simulation.slowRadius / this.rawDistance(i))
        if (isOnLeaderSight(i, i.front))
          total = total + this.flee(i.front) * (50 / this.position.distanceTo(i.front))
      }
    }

    total * simulation.avoidTheLeaderWeight
  }

  /*
 	* Functionality implemented in the beginning but didn't give good result
 	*/
  //  def avoidWall: MathVec = {
  //    val distance = 30
  //    var total = new MathVec(0, 0)
  //    if (this.position.x <= distance)
  //      total += new MathVec(1, 0) * (distance / this.position.x)
  //    if (this.position.x >= simMapWidth - distance - 20) // 20 is adde
  //      total += new MathVec(-1, 0) * ((distance + 20) / (simMapWidth - position.x))
  //    if (this.position.y <= distance)
  //      total += new MathVec(0, 1) * (distance / this.position.y)
  //    if (this.position.y >= simMapHeight - distance)
  //      total += new MathVec(0, -1) * (distance / (simMapWidth - position.y))
  //
  //    total * simulation.avoidWallConstant
  //  }

  /*
   * Seek behavior.
   */
  def seek(target: MathVec) = {
    var desiredVelocity = (target - this.getPosition).normalize * speedLimit
    desiredVelocity - this.getVelocity
  }

  /*
   * The Arrive behaviour.
   *
   * The vector should have been normalized, but it was left this way to
   * make for a more realistic "Follow Pointer" movement. I will normalize the
   * vector locally when I need it to be.
   *
   */

  def arrive(target: MathVec, slowRadius: Double): MathVec = {
    var desiredVelocity = target - this.getPosition
    val distance = desiredVelocity.magnitude

    if (distance < slowRadius) {
      desiredVelocity = desiredVelocity.normalize * this.speedLimit * (distance / slowRadius)
    } else desiredVelocity = desiredVelocity.normalize * this.speedLimit
    //    } else

    desiredVelocity - this.getVelocity
  }

  /*
   * Flee behavior.
   *
   * Just seek but reversed.
   */
  def flee(target: MathVec): MathVec = seek(target) * (-1)

  /*
   * Evade behavior.
   *
   * The T multiplier of (distance/MAX_SPEED) is used.
   */
  def evade(target: Boid): MathVec = {
    val distance = this.rawDistance(target)
    val multiplier = (distance / speedLimit)
    val futurePosition = target.getPosition + target.getVelocity * multiplier
    flee(futurePosition)
  }

  /*
 	* Helper Methods
 	*/
  def rawDistance(daBoid: Boid) = this.getPosition.distanceTo(daBoid.getPosition)
  def vecDistance(daBoid: Boid) = this.getPosition - daBoid.getPosition
  def getPosition = position
  def getVelocity = velocity

}

case class LeadingBoid(sim: Simulation, pos: MathVec, vel: MathVec) extends Boid(sim, pos, vel) {
  /*
 	* List of constant taken from class Simulation.
 	*/
  var originalDir = velocity.toPolar._2
  def followPointer = sim.allowLeaderFollowPointer
  def targetForLeader = sim.targetForLeader
  def speedLimit = sim.speedLimitLeaders
  def angleChange = sim.leaderAngleChange

  /*
 	* Wander Behavior.
 	*
 	* A simple way of creating a random displacement force constrained by
 	* a circle is used.
 	*
 	*/
  def wander: MathVec = {
    val circleRadius = 2 // tested and approved constant
    val circleDistance = 3 //
    var displacement = new MathVec(0, -1) // no effects on the program

    def changeDisplacement(noice: MathVec, diff: Double) = {
      var mag = noice.magnitude
      new MathVec(Math.cos(diff) * mag, Math.sin(diff) * mag)
    }

    displacement = changeDisplacement(displacement * circleRadius, originalDir)
    this.originalDir += Math.random() * angleChange - angleChange * 0.5
    val circleCenter: MathVec = velocity.normalize * circleDistance

    circleCenter + displacement
  }

  /*
   * Determine the tail and front for the FollowingBoids to follow and avoid, respectively.
   */
  def tail: MathVec = {
    this.getPosition - (this.getVelocity).normalize * simulation.distanceToLeader
  }
  def front: MathVec = {
    this.getPosition + (this.getVelocity).normalize * sim.spaceInFrontLeader
  }

  /*
   * Final step: adding all the needed force together.
   * Update 1.2: "Follow Pointer" functionality is added.
   */
  def steering = {
    if (followPointer == true) this.avoidLeader + this.arrive(targetForLeader, sim.slowRadius)
    else this.avoidLeader + this.wander
  }

}

case class FollowingBoid(sim: Simulation, pos: MathVec, vel: MathVec) extends Boid(sim, pos, vel) {
  /*
   * List of constants. Some are taken from Simulation class.
   */
  var originalDir = velocity.toPolar._2
  var neighbor: Buffer[FollowingBoid] = Buffer.empty
  var leader: LeadingBoid = null
  def speedLimit = sim.speedLimitFollowers

  /*
   * A method used to constantly update the neighbor of a certain radius,
   * figuring out all the Following Boids in the neighbor. Also perform the
   * search for a leader.
   */
  def updateNeighborLeader: (Buffer[FollowingBoid], LeadingBoid) = {
    neighbor.clear()
    for (i <- simulation.allFollowingBoids) {
      if (i != this) {
        if (this.rawDistance(i) <= simulation.neighborRadius) {
          neighbor = neighbor :+ i
        }
      }
    }

    leader = {
      if (!simulation.allLeadingBoids.isEmpty)
        simulation.allLeadingBoids.minBy(x => this.rawDistance(x))
      else null
    }

    (neighbor, leader)
  }

  /*
   * Follow the Leader behavior.
   *
   * It consists of an arrival vector aimed at the tail of the leader.
   */
  def followTheLeader: MathVec = {
    var total = new MathVec(0, 0)
    if (leader != null) {
      total += (this.arrive(leader.tail, simulation.slowRadius))
      // each other
    }

    total * simulation.followTheLeaderWeight
  }

  /*
   * Separation behavior.
   *
   * Aside from the normal part, there's also a multiplier that get exponentially
   * bigger when two boids get closer.
   */
  def separate: MathVec = {
    var total = new MathVec(0, 0)

    neighbor.foreach(x => total += x.vecDistance(this) * (simulation.neighborRadius / x.rawDistance(this)))
    total *= (-1)

    total * simulation.separationWeight
  }

  /*
   * Aggregate all the forces while constantly update the neighbor as well as assigned leader
   */
  def steering: MathVec = {
    updateNeighborLeader
    this.separate + this.followTheLeader + this.avoidLeader
  }
}