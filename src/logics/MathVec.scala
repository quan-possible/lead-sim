package logics

import scala.util.Random
import scala.math._

/*
 * A case class for vectors and vectors' operations.
 */

case class MathVec(x: Double, y: Double) {

  def +(another: MathVec): MathVec = {
    new MathVec(this.x + another.x, this.y + another.y)
  }

  def -(another: MathVec): MathVec = {
    new MathVec(this.x - another.x, this.y - another.y)
  }

  def *(another: Double): MathVec = {
    new MathVec(this.x * another, this.y * another)
  }

  def /(another: Double): MathVec = {
    new MathVec(this.x / another, this.y / another)
  }

  def inverse(another: Double): MathVec = {
    new MathVec(another / this.x, another / this.y)
  }

  def negate = new MathVec(-this.x, -this.y)

  def normalize: MathVec = this / (this.magnitude)

  def magnitude: Double = sqrt(this.x * this.x + this.y * this.y)

  def toPolar: (Double, Double) = (magnitude, Math.atan2(y, x))

  def bound(xBound: Int, shapeWidth: Int, yBound: Int, shapeHeight: Int) = {
    val newX =
      if (x >= xBound + shapeWidth)
        x - xBound - 2 * shapeWidth
      else if (x < -shapeWidth)
        x + xBound + 2 * shapeWidth
      else x

    val newY =
      if (y >= yBound + shapeHeight)
        y - yBound - 2 * shapeHeight
      else if (y < -shapeHeight)
        y + yBound + 2 * shapeHeight
      else y

    if (newX != x || newY != y)
      MathVec(newX, newY)
    else
      this
  }

  def truncate(constant: Double): MathVec = {
    if (this.magnitude > constant) {
      this.normalize * constant
    } else this
  }

  def distanceTo(loc: MathVec) = {
    (loc - this).magnitude
  }

  def findPerpendicular = {
    new MathVec(-this.x, this.y).normalize
  }

  /*
   * For testing purposes
   */
  override def toString() = {
    x.toString + "---" + y.toString
  }

  def roundTo3Decimals: MathVec = MathVec(
    (x * 1000).round / 1000.toDouble,
    (y * 1000).round / 1000.toDouble)

}

object MathVec {

  def randomize: MathVec = {
    val ranX = (Random.nextInt(100 + 100) - 100).toDouble / 100
    val ranY = (Random.nextInt(100 + 100) - 100).toDouble / 100

    def concac = new MathVec(ranX, ranY)
    concac.normalize
  }

  def polarReversed(mag: Double, theta: Double) = {
    val x = mag * Math.cos(theta)
    val y = mag * Math.sin(theta)
    new MathVec(x, y)
  }

}


