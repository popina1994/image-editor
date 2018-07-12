package rs.ac.bg.etf.zd173013m.gui.radio_operations

import rs.ac.bg.etf.zd173013m.logic.image.ImageLogic
import rs.ac.bg.etf.zd173013m.logic.operation.OperationAddedListener
import rs.ac.bg.etf.zd173013m.logic.operation.Operations._

import scala.swing.{ButtonGroup, Color, RadioButton}

class ButtonGroupOperations extends ButtonGroup with OperationAddedListener {

  val buttonMultiply = new RadioButtonOperation(OperationBinary(Var("_this"), Num(1),
          Option((a :Double, b:Double) => a * b), None), "Multiply")
  val buttonAdd =  new RadioButtonOperation(OperationBinary(Var("_this"), Num(0),
    Option((a :Double, b:Double) => a + b), None), "Addition")
  val buttonSub =  new RadioButtonOperation(OperationBinary(Var("_this"), Num(0),
    Option((a :Double, b:Double) => a - b), None), "Subtraction")
  val buttonInvSub =  new RadioButtonOperation(OperationBinary(Var("_this"), Num(0),
    Option((a :Double, b:Double) => b - a),None), "Inverse subtraction")
  val buttonDiv =  new RadioButtonOperation(OperationBinary(Var("_this"), Num(1),
    Option((a :Double, b:Double) => a / b),None), "Division")
  val buttonInvDiv = new RadioButtonOperation(OperationBinary(Var("_this"), Num(1),
    Option((a :Double, b:Double) => b / a),None), "Inverse division")
  val buttonSet = new RadioButtonOperation(OperationSet(ColorExpression(new Color(255, 0, 0, 0))), "Set")
  val buttonPower = new RadioButtonOperation(OperationBinary(Var("_this"), Num(1),
    Option((a :Double, b:Double) => Math.pow(a, b)),None), "Power")
  val buttonLog = new RadioButtonOperation(OperationBinary(Var("_this"), Num(1),
    Option((a :Double, b:Double) => Math.log(a) / Math.log(b)),None), "Logarithm")
  val buttonAbs = new RadioButtonOperation(OperationBinary(Var("_this"), Num(1),
    Option((a :Double, b:Double) => Math.abs(a)),None), "Absolute value")
  val buttonMin = new RadioButtonOperation(OperationBinary(Var("_this"), Num(1),
    Option((a :Double, b:Double) => Math.min(a, b)),None), "Minimum")
  val buttonMax = new RadioButtonOperation(OperationBinary(Var("_this"), Num(0),
    Option((a :Double, b:Double) => Math.max(a, b)),None), "Maximum")
  val buttonInvertColor = new RadioButtonOperation(OperationBinary(Var("_this"), Num(1),
    Option((a :Double, b:Double) => 1 - a),None), "Invert color")
  val buttonGrayScale = new RadioButtonOperation(OperationGrayScale(Var("_this")), "GrayScale")
  val buttonMedian = new RadioButtonOperation(OperationMedian(Var("this"), 0), text ="Median value")
  val buttonPond = new RadioButtonOperation(OperationPond(Var("this"), Array.ofDim[(Double, Double, Double)](1, 1)),
                                          text ="Pond value")

  var listnerRadioButtonAdded : Option[RadioButtonOperationAddedListener] = None
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
  buttons += buttonPond

  def getSelected :Option[RadioButtonOperation]=
    selected match {
      case Some(operation) => return Option(operation.asInstanceOf[RadioButtonOperation])
      case None => return None
    }

  private def addRadioButton(radioButtonOperation: RadioButtonOperation): Unit = {
    buttons += radioButtonOperation
    listnerRadioButtonAdded match {
      case Some(operationAddedListener) =>
        operationAddedListener.onRadioButtonAdded(radioButtonOperation)
      case None =>
    }
  }

  override def operationAdded(name: String, list: List[Expression], isComposite: Boolean): Unit = {
    if (!isComposite)
    {
      addRadioButton(new RadioButtonOperation(OperationSequence(Var("_this"), name, list), text = name))
    }
    else
    {

      addRadioButton(new RadioButtonOperation(OperationBinary(Var("_this"), Num(0), None, Option(listOfOperationsToComposition(list))), text = name))
    }


  }
}
