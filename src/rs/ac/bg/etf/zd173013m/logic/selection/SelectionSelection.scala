package rs.ac.bg.etf.zd173013m.logic.selection

import scala.collection.mutable.ArrayBuffer

case class SelectionSelection(_name: String, var rectangles: ArrayBuffer[SelectionRectangular]) extends Selection(_name) {
  def this(rectangles: ArrayBuffer[SelectionRectangular])=
    this("Selection" + SelectionSelection.generateId().toString, rectangles)
}

object SelectionSelection {
  var id: Int = 0
  def generateId(): Int=
  {
    id +=1
    return id
  }
}
