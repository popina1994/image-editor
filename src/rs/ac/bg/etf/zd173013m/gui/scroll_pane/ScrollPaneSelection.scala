package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewSelection
import rs.ac.bg.etf.zd173013m.logic.selection.Selection

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer
import scala.swing.{Dimension, ScrollPane}

abstract class ScrollPaneSelection() {

  protected[this]var _listSelections: ArrayBuffer[Selection] = new ArrayBuffer[Selection]()
  var listViewSelection: ListViewSelection = new ListViewSelection(_listSelections)

  private[this] val _scrollPane: ScrollPane = new ScrollPane(listViewSelection)
  scrollPane.preferredSize = new Dimension(100, 500)

  def scrollPane = _scrollPane
  protected def createSelection(nameOpt: Option[String]): Selection

  protected def addSelectionToListView(selection: Selection): Unit =
  {
    _listSelections += selection
    // This is only used because refresh does not work.
    listViewSelection.listData = _listSelections
  }

  def addNewSelection(name: Option[String]): Selection=
  {
    val selection = createSelection(name)
    addSelectionToListView(selection)

    return _listSelections.last
  }

  /* This method exists because of cleaner code */
  def vectorSelections(): Vector[Selection] = {
    return _listSelections.toVector
  }

  def selectLast(): Unit =
  {
    _listSelections.last.active = true
    listViewSelection.selectIndices(_listSelections.length - 1)
  }

  def selectAll(): Unit = {
    var idx = 0
    for (it <- _listSelections)
      {
        it.active = true
        //listViewSelection.selectIndices(Array(idx))
      }
  }

  def deleteSelected()=
  {
    _listSelections --= _listSelections.filter((s: Selection)=> s.active)
    listViewSelection.listData = _listSelections
  }

  def generateNewData() = {
    _listSelections = new ArrayBuffer[Selection]()
    listViewSelection.listData = _listSelections
  }
}
