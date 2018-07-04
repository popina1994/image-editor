package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.logic.selection.Selection

import scala.collection.mutable.ArrayBuffer
import scala.swing.ListView.Renderer
import scala.swing.{Dimension, ListView, ScrollPane}

abstract class ScrollPaneSelection() {

  // TODO: Rethink what to add to case class
  var listSelections: ArrayBuffer[Selection] = new ArrayBuffer[Selection]()


  private[this] val _scrollPane: ScrollPane = new ScrollPane(new ListView(listSelections) {
    renderer = Renderer(_.name)
    preferredSize = new Dimension(50, 400)
  })
  scrollPane.preferredSize = new Dimension(100, 500)

  def scrollPane = _scrollPane
}
