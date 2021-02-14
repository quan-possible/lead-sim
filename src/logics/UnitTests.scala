package logics

import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.Buffer
import scala.math._

class UnitTests {

  @Test def testTruncate {
    val needTest1 = MathVec(9, 5)
    val truncateDegree1 = 2
    val given1 = needTest1.truncate(truncateDegree1).roundTo3Decimals
    val correct1 = MathVec(
      (9 * sqrt(2)) / sqrt(53),
      (5 * sqrt(2)) / sqrt(53)).roundTo3Decimals
    assertEquals(correct1, given1)
    
    val needTest2 = MathVec(100, 0)
    val truncateDegree2 = 10
    val given2 = needTest2.truncate(truncateDegree2).roundTo3Decimals
    val correct2 = MathVec(10,0).roundTo3Decimals
    assertEquals(correct2, given2)    
  }

  @Test def testSeek {
    val simulation = new Simulation(1600, 900)

    val boidPosition1 = MathVec(0, 0)
    val boidVelocity1 = MathVec(2, 1).normalize
    val target1 = MathVec(500, 300)
    val correct1 = MathVec(500, 300).normalize - boidVelocity1
    val boid1 = FollowingBoid(simulation, boidPosition1, boidVelocity1)
    val needTest1 = boid1.seek(target1)
    assertEquals(correct1, needTest1)

    val boidPosition2 = MathVec(1600, 0)
    val boidVelocity2 = MathVec(2, 1).normalize
    val target2 = MathVec(1000, 300)
    val correct2 = MathVec(-600, 300).normalize - boidVelocity2
    val boid2 = FollowingBoid(simulation, boidPosition2, boidVelocity2)
    val needTest2 = boid2.seek(target2)
    assertEquals(correct2, needTest2)
  }

  @Test def testUpdateNeighborLeader {
    val simulation = new Simulation(1600, 900)
    val boid1 = FollowingBoid(simulation, MathVec(100, 100), MathVec(0, 0))
    val boid2 = FollowingBoid(simulation, MathVec(130, 100), MathVec(0, 0))
    val leadingBoid = LeadingBoid(simulation, MathVec(1600, 100), MathVec(0, 0))

    simulation addBoid (boid1)
    simulation addBoid (boid2)
    simulation addBoid (leadingBoid)

    boid1.updateNeighborLeader
    assertEquals(boid2, boid1.neighbor(0))
    assertEquals(leadingBoid, boid1.leader)
  }

}