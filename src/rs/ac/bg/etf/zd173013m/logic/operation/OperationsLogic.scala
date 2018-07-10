package rs.ac.bg.etf.zd173013m.logic.operation

import rs.ac.bg.etf.zd173013m.gui.radio_operations.ButtonGroupOperations
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.ScrollPaneSelectionLayer
import rs.ac.bg.etf.zd173013m.logic.operation.Operations._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.swing.Color

class OperationsLogic (buttonGroupOperations: ButtonGroupOperations, scrollPaneSelectionLayer: ScrollPaneSelectionLayer){
  var expr: Expression = new Var("_this")
  var recordOperation = false
  var listenerOpt: Option[OperationsListener] = None
  var listenersSeqOp: ListBuffer[OperationAddedListener] = ListBuffer()

  private [this]var listSavedOperations: ListBuffer[Expression] = ListBuffer()

  private def convertStringToPonder(str : String) : Array[Array[(Double, Double, Double)]] = {
    val length = str.count(_ == '\n')
    val outputArray = Array.ofDim[(Double, Double, Double)](length, length)
    var rowIdx = 0
    for (row <- str.split("\n")) {
      var colIdx = 0
      for (col <- row.split(" ")) {
        val components =  col.split(":")
        outputArray(rowIdx)(colIdx) =
            (components(0).trim.toDouble, components(1).trim.toDouble, components(2).trim.toDouble)
        colIdx += 1
      }
      rowIdx += 1
    }
    return outputArray
  }

  private def copyListOperations(listBuffer: List[Expression]): List[Expression] = {
    var buffer : ListBuffer[Expression]= ListBuffer()
    listBuffer.foreach((e: Expression) => buffer += e.copyOverride)
    return buffer.toList
  }

  def saveSelectedOperations(arg1: Option[String], arg2: Option[Color]): Unit = {
    buttonGroupOperations.getSelected match {
      case Some(operation) =>
        val tmpExp = expr
        expr = Var("_this")
        val tmpListeners = listenersSeqOp
        listenersSeqOp = ListBuffer()
        val tmpeListenerOpt = listenerOpt
        listenerOpt = None

        executeSelectedOperations(arg1, arg2)
        listSavedOperations += expr

        expr = tmpExp
        listenersSeqOp = tmpListeners
        listenerOpt = tmpeListenerOpt
      case None =>
        println("Nothing is selected")
    }
  }

  def createSequenceOperations(arg: Option[String], isComposite: Boolean): Unit = {
    var realName = "1"
    arg match {
      case Some(name) =>
          realName = name
      case None =>
    }
    listenersSeqOp.foreach(_.operationAdded(realName, listSavedOperations.toList, isComposite))

    listSavedOperations = ListBuffer()
  }

  def executeSelectedOperations(arg1: Option[String], arg2: Option[Color]) = {
    buttonGroupOperations.getSelected match {
      case Some(operation) =>
        for (layer <- scrollPaneSelectionLayer.vectorSelections() if layer.active) {
          operation.expression match {
            case OperationBinary(_, num, func) =>
              arg1 match {
                case Some(value) =>
                  layer.expr = OperationBinary(layer.expr, Num(value.toDouble), func)
                case None =>
                  layer.expr = OperationBinary(layer.expr, num, func)
              }
            case OperationGrayScale(_) =>
              layer.expr = OperationGrayScale(layer.expr)
            case OperationMedian(_, _) =>
              arg1 match {
                case Some(value) =>
                  layer.expr = OperationMedian(layer.expr, value.toInt)
                case None =>
                  layer.expr = OperationMedian(layer.expr, 1)
              }
            case OperationPond(_, _) =>
              arg1 match {
                case Some(value) =>
                  layer.expr = OperationPond(layer.expr, convertStringToPonder(value))
                case None =>
                  println("Error")
              }
            case OperationSequence(_, name, listOperations) =>
              layer.expr = OperationSequence(layer.expr, name, copyListOperations(listOperations))
            case OperationComposite(_, name, listOperations) =>
              layer.expr = OperationComposite(layer.expr, name, copyListOperations(listOperations))
            case _ => println("Operations is not among given ones")
          }
      }
      listenerOpt match {
        case Some(listener) => listener.appliedExpression()
        case None=> println("Listener for operations logic is not set")
      }
      case None => println("Nothing selected")
    }
  }
}
