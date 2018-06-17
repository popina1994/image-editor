package rs.ac.bg.etf.zd173013m.gui

import scala.swing._
import javax.swing.{ImageIcon, JLabel}

class ImageLabel(imagePath : String)  {
  val imageIcon: ImageIcon = new ImageIcon(imagePath)
  val imageLabel = new JLabel(imageIcon)
}
