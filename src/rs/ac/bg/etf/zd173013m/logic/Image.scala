package rs.ac.bg.etf.zd173013m.logic

import java.awt.{AlphaComposite, Color}
import java.awt.image.BufferedImage

import javax.swing.ImageIcon
import rs.ac.bg.etf.zd173013m.gui.image_label.{ImageLabel, ImageLabelListener}

import scala.swing.Point

class Image (var imageLabel: ImageLabel, var iconName: String) extends ImageLabelListener {
  imageLabel.listener = this
  private[this] var _leftCorner = new Point(0, 0)
  private[this] var _rightCorner = new Point(imageLabel.icon.getIconWidth, imageLabel.icon.getIconHeight)
  def leftCorner = _leftCorner
  def rightCorner = _rightCorner
  def leftCorner_=(value: Point) = _leftCorner = value
  def rightCorner_=(value: Point) = _rightCorner = value

  var bufferedImage: BufferedImage = null

  private def convertToBufferedImage() = {
    def swapIfNecessary()={
      def swapNecessary(tuple: (Int, Int)): (Int, Int)=
      {
        if (tuple._1 > tuple._2)
          return tuple.swap
        return tuple
      }
      val pairX = swapNecessary(leftCorner.x, rightCorner.x)
      val pairY = swapNecessary(leftCorner.y, rightCorner.y)
      leftCorner.x = pairX._1
      leftCorner.y = pairY._1
      rightCorner.x = pairX._2
      rightCorner.y = pairY._2
    }
    var icon = new ImageIcon(iconName)
    bufferedImage = new BufferedImage(icon.getIconWidth, icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
    val graphics = bufferedImage.createGraphics()
    icon.paintIcon(null, graphics, 0, 0)
    val rectColor = new Color(0, 0, 255, 255)
    graphics.setColor(rectColor)
    // TODO: Understand this line
    graphics.setComposite(AlphaComposite.Src)
    swapIfNecessary()

    graphics.drawRect(leftCorner.x, leftCorner.y, rightCorner.x - leftCorner.x, rightCorner.y - leftCorner.y)
    icon = new ImageIcon(bufferedImage)
    graphics.dispose()
    imageLabel.icon = icon
  }

  def changeIcon(iconName: String) = {
    this.iconName = iconName
    this.leftCorner = new Point(0, 0)
    this.rightCorner = new Point(imageLabel.icon.getIconWidth, imageLabel.icon.getIconHeight)
    convertToBufferedImage()
  }

  override def onMouseClick(point: Point): Unit =
    {
      leftCorner = point
      convertToBufferedImage()
    }

  override def onMouseDrag(point: Point): Unit =
    {
      rightCorner = point
      convertToBufferedImage()
    }

  override def onMouseRelease(point: Point): Unit =
    {
      rightCorner = point
      convertToBufferedImage()
    }
}

object Image{
  val DefaultFileName = "C:/Users/popina/IdeaProjects/ImageEditor/assets/1.jpg"
}
