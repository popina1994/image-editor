package rs.ac.bg.etf.zd173013m.logic.layer

import rs.ac.bg.etf.zd173013m.gui.scroll_pane.ScrollPaneSelectionLayer
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewListener
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionLayer

import scala.swing.Color

class LayerLogic(val scrollPaneSelectionLayer: ScrollPaneSelectionLayer)  extends ListViewListener{
  def newSelection(name: Option[String]): Unit = {
    val newSelect: SelectionLayer =  scrollPaneSelectionLayer.addNewSelection(name)
  }
  scrollPaneSelectionLayer.listViewSelection.listenerOpt = Option(this)

  override def onSelected(): Unit = {
    println("Layer changed")
  }

  override def onUnselected(): Unit = println("Layer changed")
}
