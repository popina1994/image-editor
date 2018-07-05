package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewSelection
import rs.ac.bg.etf.zd173013m.logic.selection.Selection

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer
import scala.swing.ListView.Renderer
import scala.swing.event.SelectionChanged
import scala.swing.{Dimension, ListView, ScrollPane}

abstract class ScrollPaneSelection() {

  // TODO: Rethink what to add to case class
  protected[this]var _listSelections: ArrayBuffer[Selection] = new ArrayBuffer[Selection]()
  var listViewSelection: ListViewSelection = new ListViewSelection(_listSelections)

  private[this] val _scrollPane: ScrollPane = new ScrollPane(listViewSelection)
  scrollPane.preferredSize = new Dimension(100, 500)

  def scrollPane = _scrollPane
  protected def createSelection(nameOpt: Option[String]): Selection
  def addNewSelection(name: Option[String]): Selection=
  {
    val selection = createSelection(None)
    _listSelections += selection
    // TODO: Maybe optimization?
    // This is only used because refresh does not work.
    listViewSelection.listData = _listSelections

    return _listSelections.last
  }

  def vectorSelections(): Vector[Selection] = {
    return _listSelections.toVector
  }

  def selectLast(): Unit =
  {
    _listSelections.last.active = true
    listViewSelection.selectIndices(_listSelections.length - 1)
  }

  def deleteSelected()=
  {
    _listSelections = _listSelections.filterNot((s: Selection)=> s.active)
    listViewSelection.listData = _listSelections
  }


}
