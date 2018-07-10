package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.logic.selection.{SelectionLayer, SelectionRectangular}

import scala.collection.immutable.Vector

class ScrollPaneSelectionLayer extends ScrollPaneSelection {

  protected override def createSelection(nameOpt: Option[String]): SelectionLayer =
    nameOpt match {
      case Some(nameVal) => new SelectionLayer(nameVal)
      case None => new SelectionLayer()
    }

  override def addNewSelection(name: Option[String]): SelectionLayer =
  {
    return  super.addNewSelection(name).asInstanceOf[SelectionLayer]
  }

  override  def vectorSelections(): Vector[SelectionRectangular] = {
    return super.vectorSelections.asInstanceOf[Vector[SelectionRectangular]]
  }
}
