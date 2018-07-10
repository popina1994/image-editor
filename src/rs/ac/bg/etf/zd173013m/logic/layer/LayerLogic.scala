package rs.ac.bg.etf.zd173013m.logic.layer

import rs.ac.bg.etf.zd173013m.gui.scroll_pane.ScrollPaneSelectionLayer
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionLayer

class LayerLogic(val scrollPaneSelectionLayer: ScrollPaneSelectionLayer) {
  def newSelection(name: Option[String]): Unit = {
    val newSelect: SelectionLayer =  scrollPaneSelectionLayer.addNewSelection(name)
  }

}
