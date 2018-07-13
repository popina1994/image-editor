package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.logic.image.Image.generateBlackImage
import rs.ac.bg.etf.zd173013m.logic.selection.{Selection, SelectionLayer, SelectionRectangular, SelectionSelection}

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

class ScrollPaneSelectionLayer() extends ScrollPaneSelection {
  protected override def createSelection(nameOpt: Option[String]): SelectionLayer =
    nameOpt match {
      case Some(nameVal) => new SelectionLayer(nameVal, None)
      case None => new SelectionLayer()
    }


  protected def createSelection(nameOpt: Option[String], width: Int, height: Int): SelectionLayer =
    nameOpt match {
      case Some(nameVal) =>
        val layer = new SelectionLayer(nameVal, None)
        layer.updateImage(generateBlackImage(width, height))
        return layer
      case None =>
        val layer = new SelectionLayer()
        layer.updateImage(generateBlackImage(width, height))
        return layer
    }

  override def addNewSelection(name: Option[String]): SelectionLayer =
  {
    val newSelection = super.addNewSelection(name).asInstanceOf[SelectionLayer]
    return newSelection
  }

  def addNewSelection(name: Option[String], width: Int, height: Int): SelectionLayer =
  {
    val selection: Selection = createSelection(name, width, height)
    super.addSelectionToListView(selection)
    return  _listSelections.last.asInstanceOf[SelectionLayer]
  }

  override  def vectorSelections(): Vector[SelectionLayer] = {
    return super.vectorSelections.asInstanceOf[Vector[SelectionLayer]]
  }
}
