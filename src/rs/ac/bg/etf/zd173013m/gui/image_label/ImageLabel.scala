package rs.ac.bg.etf.zd173013m.gui.image_label

import javax.swing.ImageIcon

import scala.swing.event.{MouseDragged, MousePressed, MouseReleased}
import scala.swing.{Label, Point}

class ImageLabel(var iconName: String, var listener: ImageLabelListener) extends Label {
  icon = new ImageIcon(iconName)
  def this(iconName: String)= this(iconName, null)

  listenTo(mouse.clicks, mouse.moves)
  reactions += {
    case MousePressed(_, point, _,_, _) => {
      listener.onMouseClick(point)
      println("Pritisnuo misa")
    }
    case MouseReleased(_, point, _, _, _) => {
      println("Pustio misa")
      listener.onMouseRelease(point)
    }
    case MouseDragged(_, point, _) => {
      println("Povukao misa")
      listener.onMouseDrag(point)
    }
  }
}
