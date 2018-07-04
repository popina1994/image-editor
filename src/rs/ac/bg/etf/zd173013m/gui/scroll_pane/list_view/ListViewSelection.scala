package rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view

import rs.ac.bg.etf.zd173013m.logic.selection.{Selection, SelectionRectangular}

import scala.collection.mutable.ArrayBuffer
import scala.swing.ListView.Renderer
import scala.swing.event.SelectionChanged
import scala.swing.{Dimension, ListView}

 class ListViewSelection(items: ArrayBuffer[Selection]) extends ListView(items) {
    renderer = Renderer(_.name)
    preferredSize = new Dimension(50, 400)
    listenTo(this.selection)

    def listChanged(seq: Seq[Selection]): Unit = {
      for (item <- items) {
        val contains = seq.contains(item)
        if (item.active != contains) {
            item.active = contains
        }
      }
    }

    reactions += {
      case SelectionChanged(z) =>
      {
        if (listener != null)
          {
            listChanged(this.selection.items)
            listener.onSelected()
          }
      }
    }
    var listener: ListViewListener = null

}
