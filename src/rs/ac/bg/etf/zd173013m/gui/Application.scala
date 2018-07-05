package rs.ac.bg.etf.zd173013m.gui

import java.awt.Toolkit._
import java.io.File

import rs.ac.bg.etf.zd173013m.gui.image_label.ImageLabel
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.{ScrollPaneSelectionLayer, ScrollPaneSelectionRectangular, ScrollPaneSelectionSelection}
import rs.ac.bg.etf.zd173013m.logic.Image

import scala.swing._
import scala.swing.event.ButtonClicked

object Application extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Image Editor"
    preferredSize = getDefaultToolkit.getScreenSize
    private val imageLabel = new ImageLabel(Image.DefaultFileName)
    private val scrollPaneSelectionRectangular = new ScrollPaneSelectionRectangular()
    private val scrollPaneSelectionSelection= new ScrollPaneSelectionSelection()
    private val image = new Image(imageLabel, Image.DefaultFileName, scrollPaneSelectionRectangular)

    contents = new FlowPanel {
      contents += imageLabel
      contents += new BoxPanel(Orientation.Vertical) {
        hGap = 3
        vGap = 3
        contents += scrollPaneSelectionRectangular.scrollPane
        contents += new Button {
          text = "Delete"
          reactions += {
            case ButtonClicked(_) => scrollPaneSelectionRectangular.deleteSelected()
          }
          preferredSize = new Dimension(20, 30)
        }
      }
      contents += new BoxPanel(Orientation.Vertical) {
        hGap = 3
        vGap = 3
        contents += scrollPaneSelectionSelection.scrollPane
        contents += new Button {
          text = "Delete"
          reactions += {
            case ButtonClicked(_) => scrollPaneSelectionSelection.deleteSelected()
          }
          preferredSize = new Dimension(20, 30)
        }
      }
      contents += new ScrollPaneSelectionLayer().scrollPane
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
                image.changeIcon(chooser.selectedFile.getAbsolutePath)
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
