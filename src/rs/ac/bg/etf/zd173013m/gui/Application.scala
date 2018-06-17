package rs.ac.bg.etf.zd173013m.gui

import java.awt.Toolkit._

import javax.swing.ImageIcon

import scala.swing._

object Application extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Image Editor"
    preferredSize = getDefaultToolkit.getScreenSize
    contents = new FlowPanel {
      contents += new Button {
        text = "touch me"
      }
      contents += new Label{
        icon = new ImageIcon("C:/Users/popina/IdeaProjects/ImageEditor/assets/2.png")
      }
    }
  }
}
