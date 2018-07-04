package rs.ac.bg.etf.zd173013m.gui.scroll_pane

import rs.ac.bg.etf.zd173013m.logic.selection.SelectionRectangular

class ScrollPaneSelectionRectangular extends ScrollPaneSelection {
  override def createSelection(_name: String): SelectionRectangular=
  {
    if (_name == null) new SelectionRectangular () else new SelectionRectangular (_name)
  }
}
