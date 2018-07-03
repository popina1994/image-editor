package rs.ac.bg.etf.zd173013m.gui

import java.awt.{AlphaComposite, Color, Graphics, Graphics2D}
import java.awt.Toolkit._
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage

import javax.swing.ImageIcon
import java.io.File

import scala.swing.ListView.Renderer
import scala.swing._
import scala.swing.event._

object Application extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Image Editor"
    preferredSize = getDefaultToolkit.getScreenSize
    private val pictureLabel = new ImageLabel ()

    case class City(name: String, country: String, population: Int, capital: Boolean)

    val items = List(
      City("Lausanne", "Switzerland", 129273, false),
      City("Paris", "France", 2203817, true),
      City("New York", "USA", 8363710, false),
      City("Berlin", "Germany", 3416300, true),
      City("Tokio", "Japan", 12787981, true)
    )
    contents = new FlowPanel {
      contents += pictureLabel
      contents += new ScrollPane(new ListView(items) {
        renderer = Renderer(_.name)
      })

    }
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(new Action("Open")
          {
            def apply
            {
              println("Open")
              val chooser = new FileChooser(new File("C:/Users/popina/IdeaProjects/ImageEditor/assets/"))
              val result = chooser.showOpenDialog(null)
              if (result == FileChooser.Result.Approve) {
                println(chooser.selectedFile)
                pictureLabel.changeIcon(chooser.selectedFile.getAbsolutePath)
              }
            }
          })
        contents += new MenuItem(new Action("Save") {
          override def apply(): Unit = {
            println("Save")
          }
        })
      }
    }
  }
}
