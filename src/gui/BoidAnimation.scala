package gui

import scala.swing._

import logics.Boid
import logics._
import java.awt.{ Color, Polygon, Shape, Graphics2D }
import java.awt.geom._

/*
 * The class in charge of drawing individual Boids.
 */

class BoidAnimation(daBoid: Boid) {
  def allowDrawForce = daBoid.simulation.allowDrawForce
  val forceLength = 0.09
  
  /*
 	* The shape of individual boids
 	*/
  val shape: Shape = {
    new Polygon(Array(-8, 8, -8), Array(-6, 0, 6), 3)
  }

  /*
 	* The shape of force vectors. Calculated as boid's position +
 	* boid's steering
 	*/
  def shapeOfForce: Shape = {
    val tip = daBoid.position + daBoid.steering * forceLength

    new Line2D.Double(daBoid.position.x, daBoid.position.y,
      tip.x, tip.y)
  }

  /*
 	* The main part that is in charge of painting the boids.
 	* If [LeadingBoid], the color of boid's body will be gray,
 	* and the color of force vector will be green. On the other hand,
 	* The [FollowingBoid] will have its body colored as white, its
 	* force vector as pink.
 	*/
  def paint(g: Graphics2D) = {
    val oldTransform = g.getTransform()

    if (allowDrawForce == true) {
      if (daBoid.isInstanceOf[LeadingBoid]) {
        g.setColor(new Color(217, 254, 214))
        g.draw(shapeOfForce)
      } else {
        g.setColor(Color.PINK)
        g.draw(shapeOfForce)
      }
    }

    if (daBoid.isInstanceOf[LeadingBoid]) {
      g.setColor(Color.DARK_GRAY)
    } else {
      g.setColor(Color.white)
    }
    g.translate(daBoid.position.x, daBoid.position.y)
    g.rotate(daBoid.velocity.toPolar._2)
    g.fill(shape)

    g.setTransform(oldTransform)
  }
}