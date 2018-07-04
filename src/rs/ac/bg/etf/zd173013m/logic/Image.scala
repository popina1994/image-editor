package rs.ac.bg.etf.zd173013m.logic

import java.awt.{AlphaComposite, Color}
import java.awt.image.BufferedImage

import javax.swing.ImageIcon
import rs.ac.bg.etf.zd173013m.gui.image_label.{ImageLabel, ImageLabelListener}
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.ScrollPaneSelectionRectangular
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewListener
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionRectangular

import scala.swing.Point

class Image (var imageLabel: ImageLabel, var iconPath: String,
             var scrollPaneSelectionRectangular: ScrollPaneSelectionRectangular)
            extends ImageLabelListener with  ListViewListener{
  imageLabel.listener = this
  scrollPaneSelectionRectangular.listViewSelection.listener = this
  var rectangle: Rectangle = new Rectangle(
                              new Point(0,0),
                              new Point(imageLabel.icon.getIconWidth, imageLabel.icon.getIconHeight))

  var bufferedImage: BufferedImage = null

  private def updateImage() = {
    var icon = new ImageIcon(iconPath)
    bufferedImage = new BufferedImage(icon.getIconWidth, icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
    val graphics = bufferedImage.createGraphics()
    icon.paintIcon(null, graphics, 0, 0)
    val rectColor = new Color(0, 0, 255, 255)
    graphics.setColor(rectColor)
    // TODO: Understand this line
    graphics.setComposite(AlphaComposite.Src)
    rectangle.order()

    def drawRectangle(rect: Rectangle)={
      graphics.drawRect(rect.leftTop.x, rect.leftTop.y,rect.rightBottom.x - rect.leftTop.x,
        rect.rightBottom.y - rect.leftTop.y)
    }

    def drawActiveRectangles()=
      for (it <- scrollPaneSelectionRectangular.listSelections
        if it.active)
        {
          val sel: SelectionRectangular = it.asInstanceOf[SelectionRectangular]
          drawRectangle(sel.rectangle)
        }

    icon = new ImageIcon(bufferedImage)
    drawRectangle(rectangle)
    drawActiveRectangles()
    graphics.dispose()
    imageLabel.icon = icon
  }

  def changeIcon(iconPath: String) = {
    this.iconPath = iconPath
    rectangle.leftTop = new Point(0, 0)
    rectangle.rightBottom = new Point(imageLabel.icon.getIconWidth, imageLabel.icon.getIconHeight)
    updateImage()
  }

  override def onMouseClick(point: Point): Unit =
    {
      rectangle.leftTop = point
      updateImage()
    }

  override def onMouseDrag(point: Point): Unit =
    {
      rectangle.rightBottom = point
      updateImage()
    }

  override def onMouseRelease(point: Point): Unit =
    {
      rectangle.rightBottom = point

      val selection: SelectionRectangular =
        scrollPaneSelectionRectangular.addNewSelection().asInstanceOf[SelectionRectangular]
      selection.rectangle = rectangle
      rectangle = new Rectangle(new Point(0, 0), new Point(imageLabel.icon.getIconWidth, imageLabel.icon.getIconHeight))
      updateImage()
    }

  override def onSelected(): Unit = updateImage()

  override def onUnselected(): Unit = updateImage()
}

object Image{
  val DefaultFileName = "C:/Users/popina/IdeaProjects/ImageEditor/assets/1.jpg"
}
