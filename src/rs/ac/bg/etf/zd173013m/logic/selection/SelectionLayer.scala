package rs.ac.bg.etf.zd173013m.logic.selection

import rs.ac.bg.etf.zd173013m.logic.image.{Image, Rectangle}
import rs.ac.bg.etf.zd173013m.logic.operation.Operations.{ColorExpression, Expression, Var}

import scala.swing.Color

class SelectionLayer (_name: String, var imageOpt: Option[Image]) extends Selection(_name) {
  def updateTransparency(alpha: Double) = {
    imageOpt match {
      case None =>
      case Some(image) =>
        for (row <- 0 until image.icon.getIconHeight; col <- 0 until image.icon.getIconWidth) {
          image.setADouble(row, col, alpha)
        }
    }
  }

  def this()= this("Layer" + SelectionLayer.generateId().toString, None)
  var expr: Expression = null
  imageOpt match {
    case None => expr = ColorExpression(new Color(255,255, 255, 255))
    case Some(image) => expr = Var("_this")
  }
  // This is only used in cases when we load/unload 0th layer
  def updateImage(image: Image): Unit = {
    expr = Var("_this")
    imageOpt = Option(image)
  }

  def calculateSelectedPixels() =
  {
    imageOpt match {
      case Some(image) =>
        expr.calculateSelectedPixels(image)
      case None =>
        println("Nothing to update")
    }
  }

  def get256Array():Option[Array[Int]] = {
    imageOpt match {
      case Some(image) =>
        return Option(image.get256RGBArray)
      case None=>
        return None
    }
  }

  def resetSelectionPixels() = {
    imageOpt match {
      case Some(image)=>
        image.resetSelection()
      case None=>
        println("Image is not initialized")
    }
  }

  def setSelectedPixels(rectange: Rectangle): Unit = {
    imageOpt match {
      case Some(image)=>
        image.setSelected(rectange)
      case None => println("Image is not initialized")
    }
  }
}

object SelectionLayer {
  var id: Int = 0
  def generateId(): Int=
  {
    id +=1
    return id
  }
}
