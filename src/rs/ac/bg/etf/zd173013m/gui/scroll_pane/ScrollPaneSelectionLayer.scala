package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.logic.image.{Image, ImageLogic}
import rs.ac.bg.etf.zd173013m.logic.selection.{SelectionLayer, SelectionRectangular}

import scala.collection.immutable.Vector

class ScrollPaneSelectionLayer() extends ScrollPaneSelection {
  protected override def createSelection(nameOpt: Option[String]): SelectionLayer =
    nameOpt match {
      case Some(nameVal) => new SelectionLayer(nameVal, None)
      case None => new SelectionLayer()
    }

  override def addNewSelection(name: Option[String]): SelectionLayer =
  {
    val newSelection = super.addNewSelection(name).asInstanceOf[SelectionLayer]
    newSelection.updateImage(Image.generateBlackImage())
    return newSelection
  }

  override  def vectorSelections(): Vector[SelectionLayer] = {
    return super.vectorSelections.asInstanceOf[Vector[SelectionLayer]]
  }
}

object ScrollPaneSelectionLayer {
  val DefaultLayerName = "default"
}
