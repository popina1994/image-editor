package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewSelection
import rs.ac.bg.etf.zd173013m.logic.selection.Selection

import scala.collection.mutable.ArrayBuffer
import scala.swing.ListView.Renderer
import scala.swing.event.SelectionChanged
import scala.swing.{Dimension, ListView, ScrollPane}

abstract class ScrollPaneSelection() {

  // TODO: Rethink what to add to case class
  var listSelections: ArrayBuffer[Selection] = new ArrayBuffer[Selection]()
  var listViewSelection: ListViewSelection = new ListViewSelection(listSelections)

  private[this] val _scrollPane: ScrollPane = new ScrollPane(listViewSelection)
  scrollPane.preferredSize = new Dimension(100, 500)

  def scrollPane = _scrollPane
  protected def createSelection(name:String = null): Selection
  def addNewSelection(name:String = null): Selection=
  {
    val selection = createSelection()
    listSelections += selection
    // TODO: Maybe optimization?
    // This is only used because refresh does not work.
    listViewSelection.listData = listSelections

    return listSelections.last
  }
}
