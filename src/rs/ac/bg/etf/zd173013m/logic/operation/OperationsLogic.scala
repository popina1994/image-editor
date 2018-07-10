package rs.ac.bg.etf.zd173013m.logic.operation

import rs.ac.bg.etf.zd173013m.gui.radio_operations.ButtonGroupOperations
import rs.ac.bg.etf.zd173013m.logic.operation.Operations._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.swing.Color

class OperationsLogic (buttonGroupOperations: ButtonGroupOperations){
  var listenerOpt: Option[OperationsListener] = None
  var expr: Expression = new Var("_this")
  var recordOperation = false
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
        operation.expression match {
          case OperationMultiply(_, _) =>
            arg1 match {
              case Some(value) =>
                expr = OperationMultiply(expr, Num(value.toDouble))
              case None =>
                expr = OperationMultiply(expr, Num(1))
            }
          case OperationAdd(_, _) =>
            arg1 match {
              case Some(value) =>
                expr = OperationAdd(expr, Num(value.toDouble))
              case None =>
                expr = OperationAdd(expr, Num(0))
            }
          case OperationSub(_, _) =>
            arg1 match  {
              case Some(value) =>
                expr = OperationSub(expr, Num(value.toDouble))
              case None =>
                expr = OperationSub(expr, Num(0))
            }
          case OperationInvSub(_, _) =>
            arg1 match  {
              case Some(value) =>
                expr = OperationInvSub(expr, Num(value.toDouble))
              case None =>
                expr = OperationInvSub(expr, Num(0))
            }
          case OperationDiv(_, _) =>
            arg1 match  {
              case Some(value) =>
                expr = OperationDiv(expr, Num(value.toDouble))
              case None =>
                expr = OperationDiv(expr, Num(1))
            }
          case OperationInvDiv(_, _) =>
            arg1 match  {
              case Some(value) =>
                expr = OperationInvDiv(expr, Num(value.toDouble))
              case None =>
                expr = OperationInvDiv(expr, Num(1))
            }
          case OperationSet(_) =>
            arg2 match {
              case Some(value) =>
                expr = OperationSet(ColorExpression(value))
              case None =>
                expr = OperationSet(ColorExpression(new Color(0, 0, 0, 0)))
            }
          case OperationPower(_, _) =>
            arg1 match  {
              case Some(value) =>
                expr = OperationPower(expr, Num(value.toDouble))
              case None =>
                expr = OperationPower(expr, Num(1))
            }
          case OperationLog(_, _) =>
            arg1 match  {
              case Some(value) =>
                expr = OperationLog(expr, Num(value.toDouble))
              case None =>
                expr = OperationLog(expr, Num(2))
            }
          case OperationAbs(_) =>
            arg1 match  {
              case Some(value) =>
                expr = OperationAbs(expr)
              case None =>
                expr = OperationAbs(expr)
            }
          case OperationMin(_, _) =>
            arg1 match {
              case Some(value) =>
                expr = OperationMin(expr, Num(value.toDouble))
              case None =>
                expr = OperationMin(expr, Num(1))
            }
          case OperationMax(_, _) =>
            arg1 match {
              case Some(value) =>
                expr = OperationMax(expr, Num(value.toDouble))
              case None =>
                expr = OperationMax(expr, Num(1))
            }
          case OperationInvertColor(_) =>
            expr = OperationInvertColor(expr)
          case OperationGrayScale(_) =>
            expr = OperationGrayScale(expr)
          case OperationMedian(_, _) =>
            arg1 match {
              case Some(value) =>
                expr = OperationMedian(expr, value.toInt)
              case None =>
                expr = OperationMedian(expr, 1)
            }
          case OperationPond(_, _) =>
            arg1 match {
              case Some(value) =>
                expr = OperationPond(expr, convertStringToPonder(value))
              case None =>
                println("Error")
            }
          case OperationSequence(_, name, listOperations) =>
            expr = OperationSequence(expr, name, copyListOperations(listOperations))
          case OperationComposite(_, name, listOperations) =>
            expr = OperationComposite(expr, name, copyListOperations(listOperations))
          case _ => println("Nema medju ponudjenim")
        }
        listenerOpt match {
          case Some(listener) => listener.changedExpression(expr)
          case None=> println("Listener for operations logic is not set")
        }
      case None => println("Nothing selected")
    }
  }
}
