package rs.ac.bg.etf.zd173013m.logic.selection

import rs.ac.bg.etf.zd173013m.logic.image.Image
import rs.ac.bg.etf.zd173013m.logic.operation.Operations.{ColorExpression, Expression, Var}

import scala.swing.Color

class SelectionLayer (_name: String, var imageOpt: Option[Image]) extends Selection(_name) {
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
}

object SelectionLayer {
  var id: Int = 0
  def generateId(): Int=
  {
    id +=1
    return id
  }
}
