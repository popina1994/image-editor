package rs.ac.bg.etf.zd173013m.logic

object Operations {


  sealed trait Expression {
    def +(that: Expression): Expression = (this, that) match {
      case (_) => Add(this, that)
    }

    def -(that: Expression): Expression = (this, that) match {
      case (_) => Sub(this, that)
    }

    def *(that: Expression): Expression = (this, that) match {
      case (_) => OperationMultiply(this, that)
    }

    def /(that: Expression): Expression = (this, that) match {
      case (_) => Div(this, that)
    }

    def set(that: Expression): Expression = (this, that) match {
      case (_) => Div(this, that)
    }

    def unary_! : Int = {
      return 0
    }

  }

  case class Var(name: String) extends Expression {
    override def toString: String = name
  }

  case class Add(e1: Expression, e2: Expression) extends  Expression {
    override def toString: String = e1.toString + " + " + e2.toString
  }

  case class Sub(e1: Expression, e2: Expression) extends  Expression {
    override def toString = e1.toString + " - " + e2.toString
  }

  case class OperationMultiply(e1: Expression, e2: Expression) extends Expression {
    override def toString = e1.toString + " * " + e2.toString
  }

  case class Div(e1: Expression, e2: Expression) extends Expression {
    override def toString = e1.toString + " / " + e2.toString
  }


  case class Num(value: Float) extends  Expression {
    override def toString = value.toString
  }

  case class SeqOp(name: String, list: List[Expression]) extends Expression
  {
    override def toString:String = {
      var output: String = "list_" + name
      list.foreach(output += " " + _)
      return output
    }
  }

}

