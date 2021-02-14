package gui

import java.awt.MouseInfo
import scala.io.Source._
import scala.swing._
import scala.swing.Label
import javax.swing.{ JRadioButton, JFormattedTextField }
import logics._
import javax.swing.filechooser.{ FileFilter, FileNameExtensionFilter }
import java.awt.{ Dimension }
import java.awt.event._
import javax.swing.JFileChooser
import java.io.{ File }
import scala.swing.event._
import scala.collection.mutable._
import scala.util.Random._
import scala.swing.Dialog._
import java.util.{ Timer, TimerTask }
import scala.util.Try._
import java.io.IOException

object MainUI extends SimpleSwingApplication {
  /*
 	* List of minimum and maximum sizes of the program
 	*/
  val minimumWidth = 500
  val minimumHeight = 500
  val maximumWidth = 1600
  val maximumHeight = 900

  def top = new MainFrame {
    val simulation = newBlankProgram
    this.operate(simulation)

    /*
 		* The main body of the program.
	  *
 		*/
    def operate(simulation: Simulation): Unit = {
      this.minimumSize = new Dimension(simulation.width, simulation.height)
      /*
 			* Assign the threads of all needed components to variables.
 			*/
      val daThread = new Thread(simulation)
      val simMap = new SimMap(simulation)
      val daUiThread = new Thread(simMap)

      title = "Follow the Leader Simulation"
      resizable = false
      /**
       * The component declares here the minimum, maximum and preferred sizes, which Layout Manager
       * possibly can follow when positioning components on the screen.
       */

      visible = true
      /*
 			* Most important panels in the program.
 			* allBox contains all the rest of the panels.
 			* allButtons contains the panel with the slider and
 			* interactive buttons.
 			* topMenu contains the MenuBar on the top of the program.
 			*/

      val allBox = new BorderPanel
      val allButtons = new GridPanel(3, 1)
      val topMenu = new MenuBar

      contents = allBox
      allBox.layout += (topMenu -> BorderPanel.Position.North)
      allBox.layout += (simMap -> BorderPanel.Position.Center)
      allBox.layout += (allButtons -> BorderPanel.Position.South)
      allBox.visible = true

      /*
 * 		Adding components to the top menu.
 */
      val file = new Menu("File")
      val openSim = new MenuItem("Open...")
      val saveSim = new MenuItem("Save...")
      val newSim = new MenuItem("New Simulation")
      val help = new Menu("Help")
      val manual = new MenuItem("Manual")
      val about = new Menu("About")

      topMenu.contents += file
      topMenu.contents += about

      file.contents += openSim
      file.contents += saveSim
      file.contents += newSim
      about.contents += manual

      /*
       * Save simulations to files. However, the implementation only saves to the
       * program's directory. Interact with object IO.
       */
      saveSim.action = {
        val j = new JFileChooser("src")
        j.setDialogTitle("Choose a file to save")
        Action("Save...") {
          val fileName = requestString("Please specify the name of the file", n => !n.trim.isEmpty() && n.size <= 261,
            "You have not specified the name of the file", null)
          fileName match {
            case Some(x) => {
              IO.saveSim(x, simulation)
              Dialog.showConfirmation(null, "File saved successfully \n You can now find it at the directory of the program",
                null, Options.OkCancel, Message.Info)
            }
            case None =>
          }

        }
      }
      manual.action = {
        val j = new Dialog {
          this.minimumSize = new Dimension(200, 200)
          val daText = new TextArea {
            this.append("This is brought to you by a handsome Vietnamese.")
          }
          contents = daText
        }
        Action("Author") { j.open() }
      }

      /*
 			* All the components of allButtons.
 			*/

      val upperRow = new FlowPanel
      val middleRow = new FlowPanel
      val lowerRow = new FlowPanel

      /*
       * The text field used to change separation weight parameter
       */

      val leaderAngleChangeConstant = new TextField(simulation.leaderAngleChange.toString()) {
        listenTo(this)
        reactions += {
          case e: EditDone => {
            simulation.leaderAngleChange = toNumber(text).getOrElse(simulation.leaderAngleChange)
          }
        }
      }
      val leaderAngleChangeLabel = new Label("Leader Turn Angle")
      val leaderAngleChangePanel = new BoxPanel(scala.swing.Orientation.Vertical)
      leaderAngleChangePanel.contents += leaderAngleChangeLabel
      leaderAngleChangePanel.contents += leaderAngleChangeConstant

      /*
       * The text field used to change separation weight parameter
       */
      val leaderSpeedConstant = new TextField(simulation.speedLimitLeaders.toString()) {
        listenTo(this)
        reactions += {
          case e: EditDone => {
            simulation.speedLimitLeaders = toNumber(text).getOrElse(simulation.speedLimitLeaders)
          }
        }
      }
      val leaderSpeedLabel = new Label("Leader Speed")
      val leaderSpeedPanel = new BoxPanel(scala.swing.Orientation.Vertical)
      leaderSpeedPanel.contents += leaderSpeedLabel
      leaderSpeedPanel.contents += leaderSpeedConstant
      /*
       * The text field used to change separation weight parameter
       */

      val separationConstant = new TextField(simulation.neighborRadius.toString()) {
        listenTo(this)
        reactions += {
          case e: EditDone => {
            simulation.neighborRadius = toNumber(text).getOrElse(simulation.neighborRadius)
          }
        }
      }
      val separationLabel = new Label("Separation Radius")
      val separationPanel = new BoxPanel(scala.swing.Orientation.Vertical)
      separationPanel.contents += separationLabel
      separationPanel.contents += separationConstant

      /*
       * The text field used to change separation distance to leader parameter.
       */
      val distanceConstant = new TextField(simulation.distanceToLeader.toString()) {
        listenTo(this)
        reactions += {
          case e: EditDone => {
            simulation.distanceToLeader = toNumber(text).getOrElse(simulation.distanceToLeader)
          }
        }
      }
      val distanceLabel = new Label("Leader Distance")
      val distancePanel = new BoxPanel(scala.swing.Orientation.Vertical)
      distancePanel.contents += distanceLabel
      distancePanel.contents += distanceConstant

      /*
       * The text field used to change force constant
       */
      val forceConstant = new TextField(simulation.forceConstance.toString()) {
        //        override def name_= ("force constant")
        listenTo(this)
        reactions += {
          case e: EditDone => {
            simulation.forceConstance = toNumber(text).getOrElse(simulation.forceConstance)
          }
        }
      }
      val forceLabel = new Label("Force")
      val forcePanel = new BoxPanel(scala.swing.Orientation.Vertical)
      forcePanel.contents += forceLabel
      forcePanel.contents += forceConstant

      /*
       * Toggle button for vector of forces
       */
      val forceVector = new ToggleButton("Force Vector") {
        action = Action("Force Vector") {
          simulation.changeDrawForce
        }
      }
      /*
       * Toggle button for vector of forces
       */
      val leadersFollowPointer = new ToggleButton("Follow Pointer") {
        action = Action("Follow Pointer") {
          simulation.changeLeaderFollowPointer
        }
      }
      /*
       * The group of buttons used to change mode of choosing creating boids: either
       * LeadingBoids or Following Boids.
       */
      val leaderFollowerGroup = new BoxPanel(Orientation.Vertical)
      val followerButton = new RadioButton {
        selected = true
      }
      val leaderButton = new RadioButton
      val radioGroup = new ButtonGroup(followerButton, leaderButton)
      followerButton.action = {
        Action("Following Boids") { simulation.createFollowers = true }
      }
      leaderButton.action = {
        Action("Leading Boids") { simulation.createFollowers = false }
      }
      /*
       * The slider used to change simulation speed.
       */
      val mySlider = new Slider {
        max = 10
        min = 0
        val daLabel = new Label("Simulation Speed")
        val daLabel2 = new Label("Pause")
        val daLabel3 = new Label("Max Speed")
        this.labels = Map(0 -> daLabel2, 10 -> daLabel3)

        this.name = "simulation speed"
        this.snapToTicks = true
        this.paintLabels = true
        this.paintTicks = true
        this.majorTickSpacing = 1
        this.extent = 5
        this.value
        this.listenTo(this)
      }

      leaderFollowerGroup.contents += followerButton
      leaderFollowerGroup.contents += leaderButton

      upperRow.contents += separationPanel
      upperRow.contents += distancePanel
      upperRow.contents += leaderSpeedPanel
      upperRow.contents += leaderAngleChangePanel

      lowerRow.contents += leaderFollowerGroup
      lowerRow.contents += mySlider
      middleRow.contents += forceVector
      middleRow.contents += leadersFollowPointer

      allButtons.contents += upperRow
      allButtons.contents += middleRow
      allButtons.contents += lowerRow

      /*
 			* All the pieces put together.
 			*/

      reactions += {
        case scala.swing.event.MousePressed(src, point, _, _, _) => {
          val location = new MathVec(point.x, point.y)
          simulation.addRandomBoid(location)
        }

      }

      def findMouse = {
        if (simulation.allowLeaderFollowPointer == true) {
          val location = MouseInfo.getPointerInfo.getLocation
          simulation.targetForLeader = MathVec(location.x, location.y)

        }
      }

      listenTo(simMap.mouse.clicks, mySlider)
      var listener1 = new ActionListener() {
        def actionPerformed(e: java.awt.event.ActionEvent) = {
          findMouse
          daThread.run()
          println(simulation)

        }
      }

      var listener2 = new ActionListener() {
        def actionPerformed(e: java.awt.event.ActionEvent) = {
          daUiThread.run()
        }
      }
      /*
       * Use equation to coordinate the slider speed with the
       * actual speed
       */
      def currentSpeed = mySlider.value * (-1) + 11

      /*
       * timer used to put the simulation in motion. currentSpeed is
       * the delay of the timer, or, in other words, the refresh rate of the program,
       * adjusted accordingly with the slider through the aforementioned equation.
       * Level 10 on the slider is equivalent to a delay of 1.
       */
      var timer1 = NewTimer.startTimer(listener1, currentSpeed)
      var timer2 = NewTimer.startTimer(listener2, currentSpeed)

      def stopAllTimer: Unit = {
        timer1.stop()
        timer2.stop()
      }

      def startAllTimer: Unit = {
        timer1.start()
        timer2.start()
      }
      /*
       * Modify the reactions of the slider so that it will change
       * the speed of simulation
       */

      mySlider.reactions += {
        case vc: ValueChanged => {
          if (mySlider.value == 0) {
            timer1.stop()
          } else {
            timer1 = NewTimer.startTimer(listener1, timer1, currentSpeed)
            timer2 = NewTimer.startTimer(listener2, timer2, currentSpeed)
          }
        }
      }

      /*
       * Used to create blank simulations.
       */
      newSim.action = {
        Action("New simulation") {
          stopAllTimer
          operate(MainUI.newBlankProgram)
        }
      }

      /*
       * Open new simulations from files. Interact with object IO.
       */
      openSim.action = {
        val j = new JFileChooser("src")
        Action("Open...") {
          j.showOpenDialog(null)
          val daFile = j.getSelectedFile
          try {
            stopAllTimer
            val great = IO.parseSim(daFile)
            operate(great)
          } catch {
            case e: Throwable => {
              startAllTimer
              Dialog.showMessage(null, "There must be some "
                + "kind of mistake", "Error", Message.Info)
            }
          }

        }
      }
    }

    /*
		*  The end of the program
 		*
 		*/
  }
  /*
 	* Helper methods for pop-up windows
 	*/
  def requestInput[Input](prompt: String, convert: String => Input, isOK: Input => Boolean, errorMessage: String, position: Position): Option[Input] = {
    def firstRequest() = Dialog.showInput(null, prompt, prompt, Message.Question, Swing.EmptyIcon, Nil, "")
    def furtherRequest() = Dialog.showInput(null, errorMessage + "\n" + prompt, prompt, Message.Error, Swing.EmptyIcon, Nil, "")
    def inputs = firstRequest() #:: Stream.continually(furtherRequest())
    def isInvalid(inputLine: String) = scala.util.Try(!isOK(convert(inputLine))).getOrElse(true)
    val firstAcceptableInput = inputs.dropWhile(_.exists(isInvalid)).head
    firstAcceptableInput.map(convert)
  }

  sealed trait Position extends Product with Serializable {
    private[gui] def parent: Option[Component]
    private[gui] def javaParent: Component = this.parent.orNull
  }

  def forceClose[Input](request: Option[Input]): Input = {
    request match {
      case Some(x) => x
      case _       => sys.exit(0)
    }
  }

  def requestInt(prompt: String, isOK: Int => Boolean, errorMessage: String, position: Position): Option[Int] = {
    requestInput(prompt, _.toInt, isOK, errorMessage, position)
  }

  def requestString(prompt: String, isOK: String => Boolean, errorMessage: String, position: Position): Option[String] = {
    requestInput(prompt, _.trim, isOK, errorMessage, position)
  }

  def toNumber(number: String): Option[Double] = {
    def message = Dialog.showMessage(null, "Please enter a positive number.")
    try {
      val nice = number.toDouble
      if (nice >= 0) Some(nice)
      else throw new Exception
    } catch {
      case e: Exception => {
        message
        None
      }
    }

  }

  /*
 	* Prompt the user to input the size of the program
 	*/

  def newBlankProgram: Simulation = {
    val width = {
      val nice = requestInt("Select the width of the Simulation (500 <= n <= 1600)", (n => n >= minimumWidth && n <= maximumWidth),
        "Please enter an integer between 500 and 1600.", null)
      forceClose(nice)
    }

    val height = {
      val nice = requestInt("Select the height of the Simulation (500 <= n <= 900)", (n => n >= minimumHeight && n <= maximumHeight),
        "Please enter an integer between 500 and 900.", null)
      forceClose(nice)
    }
    new Simulation(width, height)
  }
}
/*
 * Object for creating new timers with ease
 */
object NewTimer {
  def startTimer(listener: ActionListener, speed: Int): javax.swing.Timer = {
    val daNewTimer = new javax.swing.Timer(speed, listener)
    daNewTimer.start()
    daNewTimer
  }

  def startTimer(listener: ActionListener, oldTimer: javax.swing.Timer, speed: Int): javax.swing.Timer = {
    oldTimer.stop()
    startTimer(listener, speed)
  }
}