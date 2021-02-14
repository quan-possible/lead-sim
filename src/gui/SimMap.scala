package gui

import scala.swing._
import gui.MainUI._
import logics._
import java.awt.{ Color, Graphics2D, RenderingHints }


/*
 * The class which represent the simulation map. It's in charge of
 * render all the boids as well as itself into graphics. The class
 * is "threadable".
 */
class SimMap(simulation: Simulation) extends Panel with Runnable {
  minimumSize = new Dimension(simulation.width, simulation.height)

  override def paintComponent(g: Graphics2D) = {

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(new Color(109, 134, 156))
    g.fillRect(0, 0, simulation.width, simulation.height)
    
    /*
     * Paint all the boids.
     */
    simulation.allBoids.foreach(x => {
      val daBoid = new BoidAnimation(x)
      daBoid.paint(g)
    })

  }

  def run =  this.repaint()

}