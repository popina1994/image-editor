package rs.ac.bg.etf.zd173013m.logic.image

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color}
import java.text.SimpleDateFormat
import java.util.Date

import javax.swing.ImageIcon
import rs.ac.bg.etf.zd173013m.gui.image_label.{ImageLabel, ImageLabelListener}
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.ScrollPaneSelectionRectangular
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.list_view.ListViewListener
import rs.ac.bg.etf.zd173013m.logic.operation.Operations.{Expression, Var}
import rs.ac.bg.etf.zd173013m.logic.operation.OperationsListener
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionRectangular

import scala.collection.mutable.ArrayBuffer
import scala.swing.Point

class ImageLogic(var imageLabel: ImageLabel, var iconPath: String,
                 var scrollPaneSelectionRectangular: ScrollPaneSelectionRectangular)
            extends ImageLabelListener with  ListViewListener with OperationsListener {
  var image: Image = new Image(iconPath, scrollPaneSelectionRectangular.vectorSelections())
  imageLabel.listenerOpt = Option(this)
  scrollPaneSelectionRectangular.listViewSelection.listenerOpt = Option(this)
  var curRectangle: Rectangle = new Rectangle(
                              new Point(0,0),
                              new Point(image.icon.getIconWidth-1, image.icon.getIconHeight-1))

  private [this]var expression: Expression = Var("_this")
  private [this] var expressionCached = false

  private def updateImage() = {
    /*
    def applyOperations() = {
      //val imageTmp = new Image(iconPath, scrollPaneSelectionRectangular.vectorSelections())

      for (row <- 0 until image.icon.getIconHeight; col <- 0 until image.icon.getIconWidth) {
        val exp = expression.calculate(imageTmp, row, col)
        image.setRGBADouble(row, col, exp)
      }
    }
    */
    expression.calculateSelectedPixels(image)

    val bufferedImage = new BufferedImage(image.icon.getIconWidth, image.icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
    bufferedImage.setRGB(0, 0, image.icon.getIconWidth, image.icon.getIconHeight,
                         image.pixels, 0, image.icon.getIconWidth)
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
    updateImage()
  }

  def replaceSelections(arrayBuffer: ArrayBuffer[SelectionRectangular]) = {
    scrollPaneSelectionRectangular.replaceSelection(arrayBuffer)
    updateImage()
  }

  override def onMouseClick(point: Point): Unit =
    {
      curRectangle.leftTop = point
      updateImage()
    }

  override def onMouseDrag(point: Point): Unit =
    {
      curRectangle.rightBottom = point
      updateImage()
    }

  override def onMouseRelease(point: Point): Unit =
    {
      curRectangle.rightBottom = point
      val selection: SelectionRectangular =
        scrollPaneSelectionRectangular.addNewSelection(None)
      selection.rectangle = curRectangle
      scrollPaneSelectionRectangular.selectLast()
      curRectangle = new Rectangle(new Point(0, 0), new Point(image.icon.getIconWidth, image.icon.getIconHeight))

      updateImage()
    }

  override def onSelected(): Unit = updateImage()

  override def onUnselected(): Unit = updateImage()

   def deleteSelected(): Unit = {
     scrollPaneSelectionRectangular.deleteSelected()
     updateImage()
   }

  override def changedExpression(expression: Expression): Unit = {
    this.expression = expression
    updateImage()
  }
}

object ImageLogic{
  val DefaultFileName = "C:/Users/popina/IdeaProjects/ImageEditor/assets/1.jpg"
}
