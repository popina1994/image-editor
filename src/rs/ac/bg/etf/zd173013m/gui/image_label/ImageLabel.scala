package rs.ac.bg.etf.zd173013m.gui.image_label

import javax.swing.ImageIcon

import scala.swing.event.{MouseDragged, MousePressed, MouseReleased}
import scala.swing.{Label, Point}

class ImageLabel(var iconName: String, var listenerOpt: Option[ImageLabelListener]) extends Label {
  icon = new ImageIcon(iconName)
  def this(iconName: String)= this(iconName, None)

  listenTo(mouse.clicks, mouse.moves)
  reactions += {
    case MousePressed(_, point, _,_, _) => {
      listenerOpt match {
        case Some(listener) => listener.onMouseClick(point)
        case None =>
      }
    }
    case MouseReleased(_, point, _, _, _) => {
      listenerOpt match {
        case Some(listener) => listener.onMouseRelease(point)
        case None =>
      }

    }
    case MouseDragged(_, point, _) => {
      listenerOpt match {
        case Some(listener) => listener.onMouseDrag(point)
        case None =>
      }

    }
  }
}
