package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.logic.selection.{Selection, SelectionRectangular}

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

class ScrollPaneSelectionRectangular extends ScrollPaneSelection {


  override def createSelection(nameOpt: Option[String]): SelectionRectangular=
    nameOpt match {
      case Some(nameVal) => new SelectionRectangular(nameVal)
      case None => new SelectionRectangular()
    }

  override def addNewSelection(name: Option[String]): SelectionRectangular =
    {
      return  super.addNewSelection(name).asInstanceOf[SelectionRectangular]
    }

  override  def vectorSelections(): Vector[SelectionRectangular] = {
    return super.vectorSelections.asInstanceOf[Vector[SelectionRectangular]]
  }

  def getRectSelections(): ArrayBuffer[SelectionRectangular] = {
    return _listSelections.asInstanceOf[ArrayBuffer[SelectionRectangular]]
  }

  def replaceSelection(arrayBuffer: ArrayBuffer[SelectionRectangular]) =
  {
    _listSelections = arrayBuffer.asInstanceOf[ArrayBuffer[Selection]]
    listViewSelection.listData = _listSelections
  }
}
