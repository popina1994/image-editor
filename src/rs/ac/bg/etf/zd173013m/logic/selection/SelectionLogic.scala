package rs.ac.bg.etf.zd173013m.logic.selection

import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewListener
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.{ScrollPaneSelectionRectangular, ScrollPaneSelectionSelection}
import rs.ac.bg.etf.zd173013m.logic.image.ImageLogic

class SelectionLogic(val image: ImageLogic, val scrollPaneSelectionSelection: ScrollPaneSelectionSelection,
                     val scrollPaneSelectionRectangular: ScrollPaneSelectionRectangular) extends ListViewListener{
  scrollPaneSelectionSelection.listViewSelection.listenerOpt = Option(this)
  private var firstCreate: Boolean = true

  def newSelection(name: Option[String]) = {
    def addNewSelection() =
      scrollPaneSelectionSelection.addNewSelection(name,
        scrollPaneSelectionRectangular.getRectSelections())
    if (!firstCreate){
      scrollPaneSelectionRectangular.generateNewData()
    }
    addNewSelection()
    image.replaceSelections(scrollPaneSelectionRectangular.getRectSelections())
    firstCreate = false
  }

  private def updateSelections():Unit=
  {
    scrollPaneSelectionSelection.
      vectorSelections().
      find((s: SelectionSelection) => s.active)
    match {
      case Some(selected) => image.replaceSelections(selected.rectangles)
      case None => println("Nista nije izbrano")
    }
  }

  override def onSelected(): Unit = updateSelections()

  override def onUnselected(): Unit = updateSelections()
}
