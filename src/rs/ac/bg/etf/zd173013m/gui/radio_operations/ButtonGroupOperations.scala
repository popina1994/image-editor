package rs.ac.bg.etf.zd173013m.gui.radio_operations

import rs.ac.bg.etf.zd173013m.logic.operation.Operations._

import scala.swing.{ButtonGroup, RadioButton}

class ButtonGroupOperations extends ButtonGroup {
  val buttonMultiply = new RadioButtonOperation(OperationMultiply(Var("_this"), Num(1)), "Multiply")
  val buttonAdd = new RadioButtonOperation(OperationAdd(Var("this"), Num(value=1)), text="Addition")
  val buttonSub = new RadioButtonOperation(OperationSub(Var("this"), Num(value=0)), text ="Subtraction")
  val buttonInvSub = new RadioButtonOperation(OperationInvSub(Var("this"), Num(value=0)), text ="Inverse subtraction")
  val buttonDiv = new RadioButtonOperation(OperationDiv(Var("this"), Num(value=0)), text ="Division")
  val buttonInvDiv = new RadioButtonOperation(OperationInvDiv(Var("this"), Num(value=0)), text ="Invert division")
  val buttonSet = new RadioButtonOperation(OperationSet(Var("this")), text ="Set Color")
  val buttonPower = new RadioButtonOperation(OperationPower(Var("this"), Num(1)), text ="Power")
  val buttonLog = new RadioButtonOperation(OperationLog(Var("this"), Num(2)), text ="Log")
  val buttonAbs = new RadioButtonOperation(OperationAbs(Var("this")), text ="Absolute value")
  val buttonMin = new RadioButtonOperation(OperationMin(Var("this"), Num(1)), text ="Minimum")
  val buttonMax = new RadioButtonOperation(OperationMax(Var("this"), Num(1)), text ="Maximum")
  val buttonInvertColor = new RadioButtonOperation(OperationInvertColor(Var("this")), text ="Invert color")
  val buttonGrayScale = new RadioButtonOperation(OperationGrayScale(Var("this")), text ="GrayScale")
  val buttonMedian = new RadioButtonOperation(OperationMedian(Var("this"), 0), text ="Median value")


  buttons += buttonMultiply
  buttons += buttonAdd
  buttons += buttonSub
  buttons += buttonInvSub
  buttons += buttonDiv
  buttons += buttonInvDiv
  buttons += buttonSet
  buttons += buttonPower
  buttons += buttonLog
  buttons += buttonAbs
  buttons += buttonMin
  buttons += buttonMax
  buttons += buttonInvertColor
  buttons += buttonGrayScale
  buttons += buttonMedian

  def getSelected :Option[RadioButtonOperation]=
    selected match {
      case Some(operation) => return Option(operation.asInstanceOf[RadioButtonOperation])
      case None => return None
    }

}
