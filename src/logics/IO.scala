package logics

import java.io.File
import scala.io.Source
import scala.xml._
import scala.collection.mutable.Buffer

/*
 * An Object for reading and parsing files.
 * 
 * The program uses XML extensively.
 * 
 */

object IO {
  /*
   * The One method for parsing simulation from file.
   */
  def parseSim(daFile: File): Simulation = {
    val thisFile = xml.XML.loadFile(daFile)
    /*
     * Parsing the constructive parameter for a simulation.
     */
    def parseSimulation(node: Node) = {
      val width = (node \ "width").text.trim.toInt
      val height = (node \ "height").text.trim.toInt
      new Simulation(width, height)
    }
    /*
     * Parsing all the Boids.
     */
    def parseBoid(node: Node, simulation: Simulation) = {
      /*
       * Position first.
       */
      def parsePosition = {
        val position = (node \ "position")
        val x = (position \ "x").text.trim.toDouble
        val y = (position \ "y").text.trim.toDouble
        new MathVec(x, y)
      }
      /*
       * Then velocity.
       */
      def parseVelocity = {
        val position = (node \ "velocity")
        val x = (position \ "x").text.trim.toDouble
        val y = (position \ "y").text.trim.toDouble
        new MathVec(x, y)
      }
      /*
       * Now the boids.
       */
      (node \ "type").text.trim match {
        case "LeadingBoid" => {
          simulation.addBoid(LeadingBoid(simulation, parsePosition, parseVelocity))
        }
        case "FollowingBoid" => {
          simulation.addBoid(FollowingBoid(simulation, parsePosition, parseVelocity))
        }
        case _ =>
      }
    }
    /*
     * Putting everything together.
     */
    val simulation = parseSimulation(thisFile)
    val allBoids = thisFile \ "boid"
    allBoids.foreach(x => parseBoid(x, simulation))
    simulation
  }

  def saveSim(fileName: String, simulation: Simulation): Unit = {
    /*
     * Save the simulation.
     */
    val width = <width> { simulation.width } </width>
    val height = <height> { simulation.height } </height>
    val followingBoids = simulation.allFollowingBoids.map(x => {

      <boid>
        <type> FollowingBoid </type>
        <position>
          <x> { x.position.x } </x>
          <y> { x.position.y } </y>
        </position>
        <velocity>
          <x> { x.velocity.x } </x>
          <y> { x.velocity.y } </y>
        </velocity>
      </boid>

    })
    val leadingBoids = simulation.allLeadingBoids.map(x => {

      <boid>
        <type> LeadingBoid </type>
        <position>
          <x> { x.position.x } </x>
          <y> { x.position.y } </y>
        </position>
        <velocity>
          <x> { x.velocity.x } </x>
          <y> { x.velocity.y } </y>
        </velocity>
      </boid>

    })
    val res =
      <simulation>
        { width }
        { height }
        { followingBoids }
        { leadingBoids }
      </simulation>
    xml.XML.save(fileName + ".xml", res)
  }

}