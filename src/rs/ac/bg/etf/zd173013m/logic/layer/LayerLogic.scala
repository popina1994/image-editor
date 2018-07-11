package rs.ac.bg.etf.zd173013m.logic.layer

import rs.ac.bg.etf.zd173013m.gui.scroll_pane.ScrollPaneSelectionLayer
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewListener

class LayerLogic(val scrollPaneSelectionLayer: ScrollPaneSelectionLayer)  extends ListViewListener{
  var layerChangeListener : Option[LayerChangeListener] = None

  def newSelection(name: Option[String]): Unit = {
    scrollPaneSelectionLayer.addNewSelection(name)
  }
  scrollPaneSelectionLayer.listViewSelection.listenerOpt = Option(this)

  def updateImage(): Unit = {
    layerChangeListener match {
      case None=>
      case Some(listener) =>
        listener.onChanged()
    }
  }

  override def onSelected(): Unit = {
    println("Layer changed")
    updateImage()
  }

  def updateTransparency(alpha: Int) = {
    for (layer <- scrollPaneSelectionLayer.vectorSelections() if layer.active) {
      layer.updateTransparency(alpha / LayerLogic.MaxSlider.toDouble)
    }
    updateImage()
  }

  override def onUnselected(): Unit = {
    updateImage()
  }
}

object LayerLogic {
  val MaxSlider = 100
}
