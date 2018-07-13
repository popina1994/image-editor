package rs.ac.bg.etf.zd173013m.logic.image

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color}

import javax.swing.ImageIcon
import rs.ac.bg.etf.zd173013m.gui.image_label.{ImageLabel, ImageLabelListener}
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.{ScrollPaneSelectionLayer, ScrollPaneSelectionRectangular}
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewListener
import rs.ac.bg.etf.zd173013m.logic.layer.LayerChangeListener
import rs.ac.bg.etf.zd173013m.logic.operation.OperationsListener
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionRectangular

import scala.collection.mutable.ArrayBuffer
import java.io.File

import javax.imageio.ImageIO

import scala.swing.Point

class ImageLogic(imageLabel: ImageLabel, iconPath: String,
                 scrollPaneSelectionRectangular: ScrollPaneSelectionRectangular,
                 scrollPaneSelectionLayer: ScrollPaneSelectionLayer)
            extends ImageLabelListener with  ListViewListener with OperationsListener with LayerChangeListener{
  def saveImage(imagePath: String) =  {
    val bufferedImage = updateImage(true, false, true)
    val file = new File(imagePath)
    val ext = imagePath.split('.')
    ImageIO.write(bufferedImage, ext.last, file)

  }

  val image: Image = new Image(iconPath)

  imageLabel.listenerOpt = Option(this)
  scrollPaneSelectionRectangular.listViewSelection.listenerOpt = Option(this)

  var curRectangle: Rectangle = new Rectangle(
                              new Point(0,0),
                              new Point(image.icon.getIconWidth-1, image.icon.getIconHeight-1))

  private def updateImage(refreshImage: Boolean, updateSelection: Boolean, saveImage: Boolean):BufferedImage = {
    curRectangle.order()
    curRectangle.constraint(image.icon.getIconWidth, image.icon.getIconHeight)
    val bufferedImage = new BufferedImage(image.icon.getIconWidth, image.icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
    val graphics = bufferedImage.createGraphics()
    image.icon.paintIcon(null, graphics, 0, 0)
    if (updateSelection) {
      // TODO: one selection for all layers
      for (layer <- scrollPaneSelectionLayer.vectorSelections() if layer.active) {
        layer.resetSelectionPixels()
        if (scrollPaneSelectionRectangular.vectorSelections().isEmpty)
        {
          layer.setSelectedPixels(curRectangle)
        }
        else
        {
          for (it <- scrollPaneSelectionRectangular.vectorSelections
               if it.active) layer.setSelectedPixels(it.rectangle)
        }
      }
    }

    if (refreshImage)
    {
      for (layer <- scrollPaneSelectionLayer.vectorSelections() if layer.active)
      {
        layer.calculateSelectedPixels()
        layer.get256Array() match{
          case Some(array) =>
            val bufferedImageTmp = new BufferedImage(image.icon.getIconWidth, image.icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
            bufferedImageTmp.setRGB(0, 0, image.icon.getIconWidth, image.icon.getIconHeight,
              array, 0, image.icon.getIconWidth)
            graphics.drawImage(bufferedImageTmp, 0, 0, image.icon.getIconWidth,image.icon.getIconHeight, null)
          case None => println("Something is messed up")
        }
      }
    }

    val rectColor = ImageLogic.RectColor
    graphics.setColor(rectColor)
    graphics.setComposite(AlphaComposite.Src)

    if (!saveImage)
    {
      drawRectangle(curRectangle)
      drawActiveRectangles()
      imageLabel.icon = new ImageIcon(bufferedImage)
    }
    graphics.dispose()

    def drawRectangle(rect: Rectangle)={
      graphics.drawRect(rect.leftTop.x, rect.leftTop.y,rect.rightBottom.x - rect.leftTop.x,
        rect.rightBottom.y - rect.leftTop.y)
    }

    def drawActiveRectangles()=
      for (it <- scrollPaneSelectionRectangular.vectorSelections
           if it.active) drawRectangle(it.rectangle)

    return bufferedImage
  }

  def replaceSelections(arrayBuffer: ArrayBuffer[SelectionRectangular]) = {
    scrollPaneSelectionRectangular.replaceSelection(arrayBuffer)
    updateImage(true, true, false)
  }

  // Image label listener
  override def onMouseClick(point: Point): Unit =
    {
      curRectangle.leftTop = point
      updateImage(true, true, false)
    }

  override def onMouseDrag(point: Point): Unit =
    {
      curRectangle.rightBottom = point
      updateImage(false, false, false)
    }

  override def onMouseRelease(point: Point): Unit =
    {
      curRectangle.rightBottom = point
      val selection: SelectionRectangular =
        scrollPaneSelectionRectangular.addNewSelection(None)
      selection.rectangle = curRectangle
      scrollPaneSelectionRectangular.selectLast()
      curRectangle = new Rectangle(new Point(0, 0), new Point(image.icon.getIconWidth-1, image.icon.getIconHeight-1))

      updateImage(true, true, false)
    }

  // ListViewListener -> Selection Changed listener
  override def onSelected(): Unit = updateImage(true, true, false)

  override def onUnselected(): Unit = updateImage(true, true, false)

   def deleteSelected(): Unit = {
     scrollPaneSelectionRectangular.deleteSelected()
     updateImage(true, true, false)
   }
  // OperationsAddedListener
  override def appliedExpression(): Unit = {
    updateImage(true, true, false)
  }
  // LayerChangeListener
  override def onChanged(): Unit = {
    updateImage(true, false, false)
  }
}

object ImageLogic{
  val DefaultFileName = "C:/Users/popina/IdeaProjects/ImageEditor/assets/Initial.png"
  val DefaultBlackGenerated = "C:/Users/popina/IdeaProjects/ImageEditor/assets/Generation.png"
  val RectColor = new Color(0, 0, 255, 255)
  val LayerColor = new Color(0, 0, 0, 255)
}
