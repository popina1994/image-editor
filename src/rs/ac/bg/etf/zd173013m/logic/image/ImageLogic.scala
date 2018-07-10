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

class ImageLogic(var imageLabel: ImageLabel, var iconPath: String,
                 var scrollPaneSelectionRectangular: ScrollPaneSelectionRectangular,
                 var scrollPaneSelectionLayer: ScrollPaneSelectionLayer)
            extends ImageLabelListener with  ListViewListener with OperationsListener with LayerChangeListener{
  var image: Image = new Image(iconPath, scrollPaneSelectionRectangular.vectorSelections())
  imageLabel.listenerOpt = Option(this)
  scrollPaneSelectionRectangular.listViewSelection.listenerOpt = Option(this)
  var curRectangle: Rectangle = new Rectangle(
                              new Point(0,0),
                              new Point(image.icon.getIconWidth-1, image.icon.getIconHeight-1))

  private [this]var expression: Expression = Var("_this")

  private def updateImage(refreshImage: Boolean, updateSelection: Boolean) = {
    val bufferedImage = new BufferedImage(image.icon.getIconWidth, image.icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
    if (updateSelection) {
      for (layer <- scrollPaneSelectionLayer.vectorSelections() if layer.active) {
        // TODO: one selection for all layers
        layer.imageOpt.get.resetSelection()
        for (it <- scrollPaneSelectionRectangular.vectorSelections
             if it.active) layer.imageOpt.get.setSelected(it.rectangle)
        //layer.imageOpt.get.setSelected(curRectangle)
      }
    }
    if (refreshImage)
    {
      for (layer <- scrollPaneSelectionLayer.vectorSelections() if layer.active)
        {
          layer.expr.calculateSelectedPixels(layer.imageOpt.get)
          bufferedImage.setRGB(0, 0, image.icon.getIconWidth, image.icon.getIconHeight,
            layer.imageOpt.get.get256RGBArray, 0, image.icon.getIconWidth)
        }
    }
    for (layer <- scrollPaneSelectionLayer.vectorSelections() if layer.active)
    {
      bufferedImage.setRGB(0, 0, image.icon.getIconWidth, image.icon.getIconHeight,
        layer.imageOpt.get.get256RGBArray, 0, image.icon.getIconWidth)
    }



    val graphics = bufferedImage.createGraphics()
    val rectColor = new Color(0, 0, 255, 255)
    graphics.setColor(rectColor)
    // TODO: Understand this line
    //graphics.setComposite(AlphaComposite.Src)
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
    image = new Image(iconPath, scrollPaneSelectionRectangular.vectorSelections())
    curRectangle.leftTop = new Point(0, 0)
    curRectangle.rightBottom = new Point(image.icon.getIconWidth - 1, image.icon.getIconHeight - 1)
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
      updateImage(false, true)
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

      updateImage(false, true)
    }

  // ListViewListener -> Selection Changed listener
  override def onSelected(): Unit = updateImage(false, true)

  override def onUnselected(): Unit = updateImage(false, true)

   def deleteSelected(): Unit = {
     scrollPaneSelectionRectangular.deleteSelected()
     updateImage(false, true)
   }
  // OperationsAddedListener
  override def appliedExpression(): Unit = {
    updateImage(true, true)
  }
  // LayerChangeListener
  override def onAdded(): Unit = ???

  override def onRemoved(): Unit = ???
}

object ImageLogic{
  val DefaultFileName = "C:/Users/popina/IdeaProjects/ImageEditor/assets/Initial.png"
}
