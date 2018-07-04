package rs.ac.bg.etf.zd173013m.gui

import scala.swing
import scala.swing.{Component, Dimension, ListView, ScrollPane}
import scala.swing.ListView.Renderer

class ScrollPaneSelection() {

  // TODO: Rethink what to add to case class
  case class City(name: String, country: String, population: Int, capital: Boolean)
  val items = List(
    City("Lausanne", "Switzerland", 129273, false),
    City("Paris", "France", 2203817, true),
    City("New York", "USA", 8363710, false),
    City("Berlin", "Germany", 3416300, true),
    City("Tokio", "Japan", 12787981, true))
  private[this] val _scrollPane: ScrollPane = new ScrollPane(new ListView(items) {
    renderer = Renderer(_.name)
    preferredSize = new Dimension(100, 400)
  })
  scrollPane.preferredSize = new Dimension(200, 500)

  def scrollPane = _scrollPane
}
