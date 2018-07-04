package rs.ac.bg.etf.zd173013m.logic.selection

import rs.ac.bg.etf.zd173013m.logic.Rectangle

import scala.swing.Point

case class SelectionRectangular(_name: String) extends Selection(_name) {
  var rectangle: Rectangle = new Rectangle(new Point(0,0), new Point(0, 0))
  def this()= this("Rect" + SelectionRectangular.generateId().toString)
}

object SelectionRectangular {
  var id: Int = 0
  def generateId(): Int=
  {
    id -=1
    return id
  }
}
