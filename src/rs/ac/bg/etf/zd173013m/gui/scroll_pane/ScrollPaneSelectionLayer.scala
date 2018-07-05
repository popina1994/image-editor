package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.logic.selection.SelectionRectangular

class ScrollPaneSelectionLayer extends ScrollPaneSelection {
  _listSelections += SelectionRectangular("1l") += SelectionRectangular("2l")

  override def createSelection(name: Option[String]): SelectionRectangular = null
}
