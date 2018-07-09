package rs.ac.bg.etf.zd173013m.gui.radio_operations

import rs.ac.bg.etf.zd173013m.logic.operation.Operations.{OperationMedian, OperationPond, OperationSequence, Var}
import rs.ac.bg.etf.zd173013m.logic.operation.{OperationAddedListener, Operations, OperationsListener}

import scala.collection.mutable
import swing.{AbstractButton, BoxPanel, Orientation, RadioButton}

class BoxPanelOperationButtons(orientation: Orientation.Value, buttons: mutable.Set[AbstractButton])
  extends BoxPanel(orientation) with OperationAddedListener{
  contents ++= buttons

  override def operationAdded(name: String, list: List[Operations.Expression]): Unit = {
    val buttonOperation = new RadioButtonOperation(OperationSequence(name, list), text = name)
    contents += buttonOperation
  }
}
