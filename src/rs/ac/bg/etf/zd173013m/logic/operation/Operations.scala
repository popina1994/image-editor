package rs.ac.bg.etf.zd173013m.logic.operation

import rs.ac.bg.etf.zd173013m.logic.image.Image

import scala.swing.Color

object Operations {


  sealed trait Expression {
    // TODO: Replace with partially applied function.
    def calculate(image: Image, row: Int, col: Int): (Double, Double, Double, Double) =
    {
      var value = (0.0, 0.0, 0.0, 0.0)
      this match {
      case Var("_this") => return image.getRGBADouble(row, col)
      case Num(number) => return (number, number, number, image.getA(row, col))
      case ColorExpression(color) =>
        return (color.getRed / 256.0, color.getGreen / 256.0, color.getBlue / 256.0, color.getAlpha / 256.0)
      case OperationAdd(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        value = (exp1Res._1 + exp2Res._1, exp1Res._2 + exp2Res._2,
          exp1Res._3 + exp2Res._3, exp1Res._4)
        return value
      case OperationMultiply(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return (exp1Res._1 * exp2Res._1, exp1Res._2 * exp2Res._2,
          exp1Res._3 * exp2Res._3, exp1Res._4)
      case OperationSub(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return (exp1Res._1 - exp2Res._1, exp1Res._2 - exp2Res._2,
          exp1Res._3 - exp2Res._3, exp1Res._4)
      case OperationInvSub(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return (exp2Res._1 - exp1Res._1, exp2Res._2 - exp1Res._2,
          exp2Res._3 - exp1Res._3, exp1Res._4)
      case OperationDiv(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return (exp1Res._1 / exp2Res._1, exp1Res._2 / exp2Res._2,
          exp1Res._3 / exp2Res._3, exp1Res._4)
      case OperationInvDiv(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return (exp2Res._1 / exp1Res._1, exp2Res._2 / exp1Res._2,
          exp2Res._3 / exp1Res._3, exp1Res._4)
      case OperationSet(exp) =>
        val expRes = exp.calculate(image, row, col)
        return expRes
      case OperationPower(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return (Math.pow(exp1Res._1, exp2Res._1), Math.pow(exp1Res._2, exp2Res._2),
          Math.pow(exp1Res._3, exp2Res._3), exp1Res._4)
      case OperationLog(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return (Math.log(exp1Res._1) / Math.log(exp2Res._1), Math.log(exp1Res._2) / Math.log(exp2Res._2),
          Math.log(exp1Res._3) / Math.log(exp2Res._3), exp1Res._4)
      case OperationAbs(exp) =>
          val expRes = exp.calculate(image, row, col)
          return (Math.abs(expRes._1), Math.abs(expRes._2), Math.abs(expRes._3), expRes._4)
      case OperationMin(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return (Math.min(exp1Res._1, exp2Res._1), Math.min(exp1Res._2, exp2Res._2),
          Math.min(exp1Res._3, exp2Res._3), exp1Res._4)
      case OperationMax(exp1, exp2) =>
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return (Math.max(exp1Res._1, exp2Res._1), Math.max(exp1Res._2, exp2Res._2),
          Math.max(exp1Res._3, exp2Res._3), exp1Res._4)
      case (_) => println("Ostalo"); return (1, 1, 1, 1)
    }
  }
}

  case class Var(name: String) extends Expression {
    override def toString: String = name
  }

  case class OperationAdd(e1: Expression, e2: Expression) extends  Expression {
    override def toString: String = e1.toString + " + " + e2.toString
  }

  case class OperationSub(e1: Expression, e2: Expression) extends  Expression {
    override def toString = e1.toString + " - " + e2.toString
  }

  case class OperationInvSub(e1: Expression, e2: Expression) extends  Expression {
    override def toString = e1.toString + " - " + e2.toString
  }

  case class OperationMultiply(e1: Expression, e2: Expression) extends Expression {
    override def toString = e1.toString + " * " + e2.toString
  }

  case class OperationDiv(e1: Expression, e2: Expression) extends Expression {
    override def toString = e1.toString + " / " + e2.toString
  }
  case class OperationInvDiv(e1: Expression, e2: Expression) extends Expression {
    override def toString = e1.toString + " / " + e2.toString
  }

  case class OperationSet(e: Expression) extends Expression {
  }

  case class OperationPower(e1: Expression, e2: Expression) extends  Expression {
    override def toString = e1.toString + " ^ " + e2.toString
  }

  case class OperationLog(e1: Expression, e2: Expression) extends  Expression {
    override def toString = "Min(" + e1.toString + "," + e2.toString + ")"
  }

  case class OperationAbs(e: Expression) extends  Expression {
    override def toString = "Abs(" + e.toString + ")"
  }

  case class OperationMin(e1: Expression, e2: Expression) extends  Expression {
    override def toString = "Min(" + e1.toString + "," + e2.toString + ")"
  }

  case class OperationMax(e1: Expression, e2: Expression) extends  Expression {
    override def toString = "Mаx(" + e1.toString + "," + e2.toString + ")"
  }



  case class Num(value: Double) extends  Expression {
    override def toString = value.toString
  }

  case class ColorExpression(value: Color) extends Expression {

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

