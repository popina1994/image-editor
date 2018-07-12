package rs.ac.bg.etf.zd173013m.logic.image

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color}
import java.text.SimpleDateFormat
import java.util.Date

import javax.swing.ImageIcon
import rs.ac.bg.etf.zd173013m.gui.image_label.{ImageLabel, ImageLabelListener}
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.{ScrollPaneSelectionLayer, ScrollPaneSelectionRectangular}
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewListener
import rs.ac.bg.etf.zd173013m.logic.layer.LayerChangeListener
import rs.ac.bg.etf.zd173013m.logic.operation.Operations.{Expression, Var}
import rs.ac.bg.etf.zd173013m.logic.operation.OperationsListener
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionRectangular

import scala.collection.mutable.ArrayBuffer
import scala.swing.Point

class ImageLogic(val imageLabel: ImageLabel, var iconPath: String,
                 scrollPaneSelectionRectangular: ScrollPaneSelectionRectangular,
                 scrollPaneSelectionLayer: ScrollPaneSelectionLayer)
            extends ImageLabelListener with  ListViewListener with OperationsListener with LayerChangeListener{
  var image: Image = new Image(iconPath)

  imageLabel.listenerOpt = Option(this)
  scrollPaneSelectionRectangular.listViewSelection.listenerOpt = Option(this)

  var curRectangle: Rectangle = new Rectangle(
                              new Point(0,0),
                              new Point(image.icon.getIconWidth-1, image.icon.getIconHeight-1))

  private def updateImage(refreshImage: Boolean, updateSelection: Boolean) = {
    val bufferedImage = new BufferedImage(image.icon.getIconWidth, image.icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
    val graphics = bufferedImage.createGraphics()
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


    val rectColor = new Color(0, 0, 255, 255)
    graphics.setColor(rectColor)
    // TODO: Understand this line
    graphics.setComposite(AlphaComposite.Src)
    curRectangle.order()

    drawRectangle(curRectangle)
    drawActiveRectangles()

    graphics.dispose()

    def drawRectangle(rect: Rectangle)={
      graphics.drawRect(rect.leftTop.x, rect.leftTop.y,rect.rightBottom.x - rect.leftTop.x,
        rect.rightBottom.y - rect.leftTop.y)
    }

    def drawActiveRectangles()=
      for (it <- scrollPaneSelectionRectangular.vectorSelections
           if it.active) drawRectangle(it.rectangle)

    imageLabel.icon = new ImageIcon(bufferedImage)
  }

  def changeIcon(iconPath: String) = {
    this.iconPath = iconPath
    image = new Image(iconPath)
    curRectangle.leftTop = new Point(0, 0)
    curRectangle.rightBottom = new Point(image.icon.getIconWidth - 1, image.icon.getIconHeight - 1)
    scrollPaneSelectionLayer.vectorSelections().head.imageOpt = Option(new Image(iconPath))
    updateImage(true, true)
  }

  def replaceSelections(arrayBuffer: ArrayBuffer[SelectionRectangular]) = {
    scrollPaneSelectionRectangular.replaceSelection(arrayBuffer)
    updateImage(true, true)
  }

  // Image label listener
  override def onMouseClick(point: Point): Unit =
    {
      curRectangle.leftTop = point
      updateImage(true, true)
    }

  override def onMouseDrag(point: Point): Unit =
    {
      curRectangle.rightBottom = point
      updateImage(false, false)
    }

  override def onMouseRelease(point: Point): Unit =
    {
      curRectangle.rightBottom = point
      val selection: SelectionRectangular =
        scrollPaneSelectionRectangular.addNewSelection(None)
      selection.rectangle = curRectangle
      scrollPaneSelectionRectangular.selectLast()
      curRectangle = new Rectangle(new Point(0, 0), new Point(image.icon.getIconWidth-1, image.icon.getIconHeight-1))

      updateImage(true, true)
    }

  // ListViewListener -> Selection Changed listener
  override def onSelected(): Unit = updateImage(true, true)

  override def onUnselected(): Unit = updateImage(true, true)

   def deleteSelected(): Unit = {
     scrollPaneSelectionRectangular.deleteSelected()
     updateImage(true, true)
   }
  // OperationsAddedListener
  override def appliedExpression(): Unit = {
    updateImage(true, true)
  }
  // LayerChangeListener
  override def onChanged(): Unit = {
    updateImage(true, false)
  }
}

object ImageLogic{
  val DefaultFileName = "C:/Users/popina/IdeaProjects/ImageEditor/assets/Initial.png"
}
