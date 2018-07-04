package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.logic.selection.SelectionRectangular

class ScrollPaneSelectionSelection extends ScrollPaneSelection {
  listSelections += SelectionRectangular("1s") += SelectionRectangular("2s")

  override def createSelection(name: String): SelectionRectangular = null
}
