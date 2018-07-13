package rs.ac.bg.etf.zd173013m.logic.image

import scala.swing.Point

case class Rectangle(var leftTop: Point, var rightBottom: Point) {
  def order()= {
    def swapNecessary(tuple: (Int, Int)): (Int, Int) = {
      if (tuple._1 > tuple._2)
        return tuple.swap
      return tuple
    }

    val pairX = swapNecessary(leftTop.x, rightBottom.x)
    val pairY = swapNecessary(leftTop.y, rightBottom.y)
    leftTop.x = pairX._1
    leftTop.y = pairY._1
    rightBottom.x = pairX._2
    rightBottom.y = pairY._2
  }
  def constraint(width: Int, height: Int): Unit = {
    def clampPoint(point: Point)=
      new Point(clamp(point.x, 0, width-1), clamp(point.y, 0, height-1))
    leftTop = clampPoint(leftTop)
    rightBottom = clampPoint(rightBottom)
  }

  def clamp(value: Int, minRange: Int, maxRange: Int): Int = {
    return Math.max(minRange, Math.min(maxRange, value))
  }
}
