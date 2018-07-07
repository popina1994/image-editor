package rs.ac.bg.etf.zd173013m.logic.operation

import rs.ac.bg.etf.zd173013m.gui.radio_operations.ButtonGroupOperations
import rs.ac.bg.etf.zd173013m.logic.operation.Operations.{Expression, Num, OperationMultiply, Var}

class OperationsLogic (buttonGroupOperations: ButtonGroupOperations){
  var listenerOpt: Option[OperationsListener] = None
  var expr: Expression = new Var("_this")
  def executeSelectedOperation() = {
    buttonGroupOperations.getSelected match {
      case Some(operation) => println(operation.name)
        operation.expression match {
          case OperationMultiply(_, _) =>
            expr = OperationMultiply(Num(1), expr)
            listenerOpt match {
              case Some(listener) => listener.changedExpression(expr)
              case None=>
            }

          case _ => println("Nesto")
        }
      case None => println("Nothing selected")
    }
    // Evaluate And Update image`
  }
}
