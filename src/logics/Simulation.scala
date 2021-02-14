package logics
import scala.collection.mutable.Buffer
import java.awt.event.ActionListener
import scala.util.Random

/*
 * The most important class of Simulation. The class is "threadable".
 */
class Simulation(val width: Int, val height: Int) extends Runnable {
  /* List of constants:  */
  var avoidTheLeaderWeight: Double = 1000
  var followTheLeaderWeight: Double = 5
  var separationWeight: Double = 10
  var avoidWallConstant: Double = 50
  var neighborRadius: Double = 30
  var leaderAngleChange: Double = 0.3

  var speedLimitFollowers: Double = 1
  var speedLimitLeaders: Double = 1
  var forceConstance: Double = 0.1
  var slowRadius: Double = 10
  var distanceToLeader: Double = 50
  val spaceInFrontLeader: Double = 25
  /* End list of constants */

  var targetForLeader: MathVec = new MathVec(0, 0)
  var allowLeaderFollowPointer = false
  var allowDrawForce = false
  var createFollowers = true
  val allFollowingBoids: Buffer[FollowingBoid] = Buffer.empty
  val allLeadingBoids: Buffer[LeadingBoid] = Buffer.empty

  def allBoids: Buffer[Boid] = allFollowingBoids ++ allLeadingBoids

  /*
   * Methods for toggling buttons.
   */
  def changeDrawForce = allowDrawForce = !allowDrawForce
  def changeLeaderFollowPointer = allowLeaderFollowPointer = !allowLeaderFollowPointer

  /*
   * The simulation runs.
   */
  def step() = {
    allBoids.foreach(_.move)
  }

  def reset = {
    allFollowingBoids.clear
    allLeadingBoids.clear
  }

  /*
   * Method for adding boids. Used extensively in IO.
   */
  def addBoid(daBoid: Boid) = {
    daBoid match {
      case ngon: LeadingBoid   => allLeadingBoids += ngon
      case ngon: FollowingBoid => allFollowingBoids += ngon
    }
  }
  override val toString = this.width + "_" + this.height
  /*
   * Method for adding Boid knowing only the location.
   */
  def addRandomBoid(location: MathVec): Boid = {
    if (createFollowers == true) {
      val newBoid = new FollowingBoid(this, location, MathVec.randomize)
      allFollowingBoids += newBoid
      newBoid
    } else {
      val newBoid = new LeadingBoid(this, location, MathVec.randomize)
      allLeadingBoids += newBoid
      newBoid
    }
  }

  /*
   * Method for thread
   */
  def run() = this.step()

}