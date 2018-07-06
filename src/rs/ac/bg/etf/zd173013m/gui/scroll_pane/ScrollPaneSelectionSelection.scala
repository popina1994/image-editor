package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.logic.selection.{Selection, SelectionRectangular, SelectionSelection}

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer
import scala.swing.ListView.IntervalMode

class ScrollPaneSelectionSelection extends ScrollPaneSelection {
  // Allows only one selection at time to be on the screen.
  listViewSelection.selection.intervalMode = IntervalMode.Single

  def createSelection(nameOpt: Option[String], rectangles: ArrayBuffer[SelectionRectangular]):
  SelectionSelection = {
    nameOpt match {
      case Some(nameVal) => new SelectionSelection(nameVal, rectangles)
      case None => new SelectionSelection(rectangles)
    }
  }

  override protected def createSelection(nameOpt: Option[String]): Selection =
    throw new IllegalArgumentException("Not supported")

  override def addNewSelection(name: Option[String]): SelectionSelection =
  {
    return  super.addNewSelection(name).asInstanceOf[SelectionSelection]
  }

  def addNewSelection(name: Option[String], arrayBuffer: ArrayBuffer[SelectionRectangular]): SelectionSelection =
  {
    val selection: Selection = createSelection(name, arrayBuffer)
    super.addSelectionToListView(selection)
    return  _listSelections.last.asInstanceOf[SelectionSelection]
  }

  override  def vectorSelections(): Vector[SelectionSelection] = {
    return super.vectorSelections.asInstanceOf[Vector[SelectionSelection]]
  }
}
