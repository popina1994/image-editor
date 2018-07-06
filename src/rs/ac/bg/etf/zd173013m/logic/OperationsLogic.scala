package rs.ac.bg.etf.zd173013m.logic

import rs.ac.bg.etf.zd173013m.gui.radio_operations.ButtonGroupOperations
import rs.ac.bg.etf.zd173013m.logic.Operations.{Expression, Num, OperationMultiply, Var}

class OperationsLogic (buttonGroupOperations: ButtonGroupOperations){
  var expr: Expression = new Var("_this")
  def executeSelectedOperation() = {
    buttonGroupOperations.getSelected match {
      case Some(operation) => println(operation.name)
        operation.expression match {
          case OperationMultiply(_, _) => expr = OperationMultiply(Num(1), expr)
          case _ => println("Nesto")
        }
      case None => println("Nothing selected")
    }
  }
}
