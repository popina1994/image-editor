package rs.ac.bg.etf.zd173013m.logic.operation

import rs.ac.bg.etf.zd173013m.logic.operation.Operations.Expression

trait OperationAddedListener {
  def operationAdded(name: String, list: List[Expression], isComposite: Boolean):Unit
}
