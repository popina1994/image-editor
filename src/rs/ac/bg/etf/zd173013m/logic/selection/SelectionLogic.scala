package rs.ac.bg.etf.zd173013m.logic.selection

import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewListener
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.{ScrollPaneSelectionRectangular, ScrollPaneSelectionSelection}
import rs.ac.bg.etf.zd173013m.logic.image.ImageLogic

case class SelectionLogic(val imageLogic: ImageLogic, val scrollPaneSelectionSelection: ScrollPaneSelectionSelection,
                          val scrollPaneSelectionRectangular: ScrollPaneSelectionRectangular) extends ListViewListener{
  scrollPaneSelectionSelection.listViewSelection.listenerOpt = Option(this)

  def addNewSelection(name: Option[String]) = {
    def createNewSelection() =
      scrollPaneSelectionSelection.addNewSelection(name,
        scrollPaneSelectionRectangular.getRectSelections())
    if (!scrollPaneSelectionSelection.vectorSelections().isEmpty){
      scrollPaneSelectionRectangular.generateNewData()
    }
    createNewSelection()
    imageLogic.replaceSelections(scrollPaneSelectionRectangular.getRectSelections())
  }

  def deleteSelected() = {
    scrollPaneSelectionSelection.deleteSelected()
    scrollPaneSelectionRectangular.generateNewData()
  }

  private def updateSelections():Unit=
  {
    scrollPaneSelectionSelection.
      vectorSelections().
      find((s: SelectionSelection) => s.active)
    match {
      case Some(selected) => imageLogic.replaceSelections(selected.rectangles)
      case None => println("None selection is chosen")
    }
  }

  override def onSelected(): Unit = updateSelections()

  override def onUnselected(): Unit = updateSelections()
}
