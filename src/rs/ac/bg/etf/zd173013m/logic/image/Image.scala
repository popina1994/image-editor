package rs.ac.bg.etf.zd173013m.logic.image

import java.awt.image.{BufferedImage, DataBufferByte, DataBufferInt}

import javax.swing.ImageIcon

import scala.swing.Color

class Image(iconPath: String) {
  val icon = new ImageIcon(iconPath)
  private[this] val bufferedImage = new BufferedImage(icon.getIconWidth, icon.getIconHeight, BufferedImage.TYPE_INT_ARGB)
  private[this] val g = bufferedImage.createGraphics()
  icon.paintIcon(null, g, 0, 0)
  g.dispose()
  var pixels: Array[Int] = bufferedImage.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
  var tmpBuffer: Array[Int] = pixels.clone()

  def idx(row: Int, col: Int) = row * icon.getIconWidth + col

  def getA(row: Int, col: Int): Int = (pixels(idx(row, col)) >> 24) & 0xff;
  def getR(row: Int, col: Int): Int = (pixels(idx(row, col)) >> 16) & 0xff
  def getG(row: Int, col: Int): Int = (pixels(idx(row, col)) >> 8) & 0xff;
  def getB(row: Int, col: Int): Int = pixels(idx(row, col)) & 0xff;

  def getRGBA(row: Int, col: Int) : (Int, Int, Int, Int) = return (getR(row, col), getG(row, col), getB(row, col), getA(row, col))

  def getRGBADouble(row: Int, col: Int) : (Double, Double, Double, Double) =
    return (getR(row, col) / Image.ComponentValues,
            getG(row, col) / Image.ComponentValues,
            getB(row, col) / Image.ComponentValues,
            getA(row, col) / Image.ComponentValues)

  def setA(row: Int, col: Int, pixel: Int) = pixels(idx(row, col)) = (pixels(idx(row, col)) & (~(0xff << 24))) | (pixel << 24)

  def setR(row: Int, col: Int, pixel: Int) =
    pixels(idx(row, col)) = (pixels(idx(row, col)) & (~(0xff << 16))) | (pixel << 16)

    def setG(row: Int, col: Int, pixel: Int) =
    pixels(idx(row, col)) = (pixels(idx(row, col)) & (~(0xff << 8))) | (pixel << 8)
  def setB(row: Int, col: Int, pixel: Int) =
    pixels(idx(row, col)) = (pixels(idx(row, col)) & (~(0xff))) | pixel

  def setRGBA(row: Int, col: Int, rgba: (Int, Int, Int, Int)) = {
    setR(row, col, rgba._1)
    setG(row, col, rgba._2)
    setB(row, col, rgba._3)
    setA(row, col, rgba._4)
  }

  def setRGBADouble(row: Int, col: Int, rgba: (Double, Double, Double, Double)) = {
    setR(row, col, (rgba._1 * Image.ComponentValues).toInt)
    setG(row, col, (rgba._2 * Image.ComponentValues).toInt)
    setB(row, col, (rgba._3 * Image.ComponentValues).toInt)
    setA(row, col, (rgba._4 * Image.ComponentValues).toInt)
  }
}

object Image {
  val ComponentValues: Double = 256.0
}
