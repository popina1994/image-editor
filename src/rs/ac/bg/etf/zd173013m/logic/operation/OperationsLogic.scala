package rs.ac.bg.etf.zd173013m.logic.operation

import rs.ac.bg.etf.zd173013m.gui.radio_operations.{ButtonGroupOperations, RadioButtonOperation}
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.ScrollPaneSelectionLayer
import rs.ac.bg.etf.zd173013m.logic.operation.Operations._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.swing.Color

class OperationsLogic (buttonGroupOperations: ButtonGroupOperations, scrollPaneSelectionLayer: ScrollPaneSelectionLayer){
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
      case Some(_) =>
        val tmpListeners = listenersSeqOp
        listenersSeqOp = ListBuffer()
        val tmpListenerOpt = listenerOpt
        listenerOpt = None
        buttonGroupOperations.getSelected match {
          case Some(operation) =>
              getClickedExpression(operation, arg1, arg2, Var("_this")) match {
              case Some(expr) =>
                listSavedOperations += expr
              case None =>
            }
          case None =>
        }
        listenersSeqOp = tmpListeners
        listenerOpt = tmpListenerOpt
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

  def getClickedExpression(operation: RadioButtonOperation, arg1: Option[String], arg2: Option[Color],
                           curExpr: Expression): Option[Expression] = {

    operation.expression match {
      case OperationBinary(_, num, func) =>
        arg1 match {
          case Some(value) =>
            Option(OperationBinary(curExpr, Num(value.toDouble), func))
          case None =>
            Option(OperationBinary(curExpr, num, func))
        }
      case OperationSet(_) =>
        arg2 match {
          case Some(value) =>
            Option(OperationSet(ColorExpression(value)))
          case None =>
            Option(OperationSet(ColorExpression(new Color(255, 0, 0, 255))))
        }
      case OperationGrayScale(_) =>
        Option(OperationGrayScale(curExpr))
      case OperationMedian(_, _) =>
        arg1 match {
          case Some(value) =>
            Option(OperationMedian(curExpr, value.toInt))
          case None =>
            Option(OperationMedian(curExpr, 1))
        }
      case OperationPond(_, _) =>
        arg1 match {
          case Some(value) =>
            Option(OperationPond(curExpr, convertStringToPonder(value)))
          case None =>
            None
        }
      case OperationSequence(_, name, listOperations) =>
        Option(OperationSequence(curExpr, name, copyListOperations(listOperations)))
      case OperationComposite(_, name, listOperations) =>
        Option(OperationComposite(curExpr, name, copyListOperations(listOperations)))
      case _ => println("Operations is not among given ones")
        None
    }
  }

  def executeSelectedOperations(arg1: Option[String], arg2: Option[Color]) = {
    buttonGroupOperations.getSelected match {
      case Some(operation) =>
        for (layer <- scrollPaneSelectionLayer.vectorSelections() if layer.active) {
          getClickedExpression(operation, arg1, arg2, layer.expr) match {
            case Some(expr) =>
              layer.expr = expr
            case None => println("Something is really bad")
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
