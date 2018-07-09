package rs.ac.bg.etf.zd173013m.gui.radio_operations

import rs.ac.bg.etf.zd173013m.logic.operation.Operations.{OperationMedian, OperationPond, OperationSequence, Var}
import rs.ac.bg.etf.zd173013m.logic.operation.{OperationAddedListener, Operations, OperationsListener}

import scala.collection.mutable
import swing.{AbstractButton, BoxPanel, Orientation, RadioButton}

class BoxPanelOperationButtons(orientation: Orientation.Value, buttons: mutable.Set[AbstractButton])
  extends BoxPanel(orientation) with RadioButtonOperationAddedListener {
  contents ++= buttons

  override def onRadioButtonAdded(button: RadioButtonOperation): Unit = {
    contents += button
  }
}
