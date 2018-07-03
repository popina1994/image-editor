package rs.ac.bg.etf.zd173013m.gui

import java.awt.{AlphaComposite, Color}
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage

import javax.swing.ImageIcon

import scala.swing.{Label, Point}
import scala.swing.event.{MousePressed, MouseReleased}

class ImageLabel extends Label {
  var iconName = "C:/Users/popina/IdeaProjects/ImageEditor/assets/1.jpg"
  icon = new ImageIcon("C:/Users/popina/IdeaProjects/ImageEditor/assets/1.jpg")
  listenTo(mouse.clicks)
  private[this] var leftCorner = new Point(0, 0)
  private[this] var rightCorner = new Point(icon.getIconWidth, icon.getIconHeight)
  reactions += {
    case MousePressed(_, point, _,_, _) => {
      leftCorner = point
      println("Pritisnuo misa")
    }
    case MouseReleased(_, point, _, _, _) => {
      println("Pustio misa")
      rightCorner = point
      convertToBufferedImage()
    }
  }

  var bufferedImage: BufferedImage = null
  private def convertToBufferedImage() = {
    this.icon = new ImageIcon(iconName)
    bufferedImage = new BufferedImage(icon.getIconWidth, icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
    val graphics = bufferedImage.createGraphics()
    icon.paintIcon(null, graphics, 0, 0)
    val rectColor = new Color(0, 0, 255, 255)
    graphics.setColor(rectColor)
    // TODO: Understand this line
    graphics.setComposite(AlphaComposite.Src)
    graphics.drawRect(leftCorner.x, leftCorner.y, rightCorner.x - leftCorner.x, rightCorner.y - leftCorner.y)
    icon = new ImageIcon(bufferedImage)
    graphics.dispose()
  }

  def changeIcon(iconName: String) = {
    this.iconName = iconName
    this.icon = new ImageIcon(iconName)
    this.leftCorner = new Point(0, 0)
    this.rightCorner = new Point(icon.getIconWidth, icon.getIconHeight)
    convertToBufferedImage()
  }
}
