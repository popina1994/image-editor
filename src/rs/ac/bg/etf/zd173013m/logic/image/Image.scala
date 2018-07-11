package rs.ac.bg.etf.zd173013m.logic.image

import java.awt.image.{BufferedImage, DataBufferInt}

import javax.swing.ImageIcon
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionRectangular

import scala.swing.Color

class Image(iconPath: String, var vectorSelections: Vector[SelectionRectangular]) {
  val icon = new ImageIcon(iconPath)
  private[this] val bufferedImage = new BufferedImage(icon.getIconWidth, icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
  private[this] val g = bufferedImage.createGraphics()
  icon.paintIcon(null, g, 0, 0)
  g.dispose()

  var writeTmp = false

  private val pixels: Array[Int] = bufferedImage.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
  var pixelsComponents: Array[(Double, Double, Double, Double)] =
    Array.ofDim[(Double, Double, Double, Double)](pixels.length)
  private val selectedPixels: Array[Boolean] = for (it <- pixels) yield true

  convert256toComponents()

  private def idx(row: Int, col: Int) = row * icon.getIconWidth + col

  private def getA(row: Int, col: Int): Int = (pixels(idx(row, col)) >> 24) & 0xff;
  private def getR(row: Int, col: Int): Int = (pixels(idx(row, col)) >> 16) & 0xff
  private def getG(row: Int, col: Int): Int = (pixels(idx(row, col)) >> 8) & 0xff;
  private def getB(row: Int, col: Int): Int = pixels(idx(row, col)) & 0xff;

  def getADouble(row: Int, col: Int): Double = pixelsComponents(idx(row, col))._4

  private def getRGBADoublePixel(row: Int, col: Int) : (Double, Double, Double, Double) =
    return (getR(row, col) / Image.ComponentValues,
            getG(row, col) / Image.ComponentValues,
            getB(row, col) / Image.ComponentValues,
            getA(row, col) / Image.ComponentValues)

  def getRGBADouble(row: Int, col: Int) : (Double, Double, Double, Double) =
    return pixelsComponents(idx(row, col))

  def getRGBADoubleCheck(row: Int, col: Int) : Option[(Double, Double, Double, Double)] = {
    if ((row < 0) || (row >= icon.getIconHeight) || (col < 0) || (col >= icon.getIconWidth))
      return None
    return Option(getRGBADouble(row, col))
  }

  private def convert256toComponents():Unit=
    for (row<-0 until icon.getIconHeight)
      for (col <-0 until icon.getIconWidth)
        pixelsComponents(idx(row, col))  = getRGBADoublePixel(row, col)

  private def setAPixel(row: Int, col: Int, pixel: Int) =
    pixels(idx(row, col)) = (pixels(idx(row, col)) & (~(0xff << 24))) | (pixel << 24)

  private def setRPixel(row: Int, col: Int, pixel: Int) =
    pixels(idx(row, col)) = (pixels(idx(row, col)) & (~(0xff << 16))) | (pixel << 16)

  private def setGPixel(row: Int, col: Int, pixel: Int) =
    pixels(idx(row, col)) = (pixels(idx(row, col)) & (~(0xff << 8))) | (pixel << 8)

  private def setBPixel(row: Int, col: Int, pixel: Int) =
    pixels(idx(row, col)) = (pixels(idx(row, col)) & (~(0xff))) | pixel

  private def setRGBADoublePixel(row: Int, col: Int, rgba: (Double, Double, Double, Double)) = {
    setRPixel(row, col, (rgba._1 * Image.ComponentValues).toInt)
    setGPixel(row, col, (rgba._2 * Image.ComponentValues).toInt)
    setBPixel(row, col, (rgba._3 * Image.ComponentValues).toInt)
    setAPixel(row, col, (rgba._4 * Image.ComponentValues).toInt)
  }

  def setADouble(row: Int, col: Int, pixelDouble: Double): Unit=
    pixelsComponents(idx(row, col)) = pixelsComponents(idx(row, col)).copy(_4 = pixelDouble)

  def setRGBADouble(row: Int, col: Int, rgba: (Double, Double, Double, Double),
                    buffer: Array[(Double, Double, Double, Double)]) = {
    buffer(idx(row, col)) = rgba
  }

  def get256RGBArray :Array[Int] = {
    for (row<-0 until icon.getIconHeight)
      for (col <-0 until icon.getIconWidth)
        setRGBADoublePixel(row, col, pixelsComponents(idx(row, col)))
    return pixels
  }

  def isSelected(row: Int, col: Int) = selectedPixels(idx(row, col))
  def setSelected(row: Int, col: Int, selected: Boolean) = selectedPixels(idx(row, col)) = selected
  def resetSelection() = for (i <- 0 until selectedPixels.length) selectedPixels(i) = false

  def setSelected(rectangle: Rectangle): Unit = {
    for (row <- rectangle.leftTop.y to rectangle.rightBottom.y;
          col <- rectangle.leftTop.x to rectangle.rightBottom.x)
        setSelected(row, col, true)
  }
}

object Image {
  val ComponentValues: Double = 256.0

  def generateBlackImage():Image=
  {
    return new Image(ImageLogic.DefaultFileName, Vector())
  }
}
