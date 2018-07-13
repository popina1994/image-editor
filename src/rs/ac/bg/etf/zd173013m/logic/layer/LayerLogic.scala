package rs.ac.bg.etf.zd173013m.logic.layer

import rs.ac.bg.etf.zd173013m.gui.scroll_pane.ScrollPaneSelectionLayer
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewListener
import rs.ac.bg.etf.zd173013m.logic.image.ImageLogic

class LayerLogic(val scrollPaneSelectionLayer: ScrollPaneSelectionLayer, imageLogic: ImageLogic)  extends ListViewListener{
  var layerChangeListener : Option[LayerChangeListener] = None

  def newSelection(name: Option[String]): Unit = {
    scrollPaneSelectionLayer.addNewSelection(name, imageLogic.image.icon.getIconWidth, imageLogic.image.icon.getIconHeight)
  }
  scrollPaneSelectionLayer.listViewSelection.listenerOpt = Option(this)

  def updateImage(): Unit = {
    layerChangeListener match {
      case None=>
      case Some(listener) =>
        listener.onChanged()
    }
  }

  def updateTransparency(alpha: Int) = {
    for (layer <- scrollPaneSelectionLayer.vectorSelections() if layer.active) {
      layer.updateTransparency(alpha / LayerLogic.MaxSlider.toDouble)
    }
    updateImage()
  }

  override def onSelected(): Unit = {
    updateImage()
  }

  override def onUnselected(): Unit = {
    updateImage()
  }
}

object LayerLogic {
  val MaxSlider = 100
}
