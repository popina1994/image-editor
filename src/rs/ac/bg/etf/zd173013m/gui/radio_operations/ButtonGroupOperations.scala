package rs.ac.bg.etf.zd173013m.gui.radio_operations

import rs.ac.bg.etf.zd173013m.logic.Operations.{Num, OperationMultiply, Var}

import scala.swing.{ButtonGroup, RadioButton}

class ButtonGroupOperations extends ButtonGroup {
  val buttonMultiply = new RadioButtonOperation(OperationMultiply(new Var("_this"), new Num(1)), "Multiply")
  buttons += buttonMultiply

  def getSelected :Option[RadioButtonOperation]=
    selected match {
      case Some(operation) => return Option(operation.asInstanceOf[RadioButtonOperation])
      case None => return None
    }

}
