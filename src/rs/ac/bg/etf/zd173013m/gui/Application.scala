package rs.ac.bg.etf.zd173013m.gui

import java.awt.Toolkit._

import javax.swing.ImageIcon
import java.io.File

import scala.swing._
import scala.swing.event._

object Application extends SimpleSwingApplication {
  def top = new MainFrame {
    //TODO: Submenu/ open
    title = "Image Editor"
    preferredSize = getDefaultToolkit.getScreenSize
    private val pictureLabel = new Label{
      icon = new ImageIcon("C:/Users/popina/IdeaProjects/ImageEditor/assets/1.jpg")
      reactions += {
        case MouseClicked(_, point, _, _, _) => { }
      }
    }
    contents = new FlowPanel {
      contents += new Button {
        text = "touch me"
      }
      contents += pictureLabel
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
                pictureLabel.icon = new ImageIcon(chooser.selectedFile.getAbsolutePath)
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
