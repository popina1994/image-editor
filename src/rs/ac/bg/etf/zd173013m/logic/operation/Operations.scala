package rs.ac.bg.etf.zd173013m.logic.operation

import rs.ac.bg.etf.zd173013m.logic.image.Image
import rs.ac.bg.etf.zd173013m.logic.operation.Operations._

import scala.swing.Color

object Operations {


  sealed trait Expression {
    // TODO: Replace with partially applied function.
    def calculate(image: Image, row: Int, col: Int): (Double, Double, Double, Double) =
    {

      def applyFunToRGB(inv: Boolean)(val1: (Double, Double, Double, Double), val2: (Double, Double, Double, Double),
                        fun: (Double, Double)=> Double): (Double, Double, Double, Double) =
      {
        val alpha = if (!inv) val1._4 else val2._4
        return (fun(val1._1, val2._1), fun(val1._2, val2._2), fun(val1._3, val2._3), alpha)
      }

      def applyFunCurry(image: Image, row: Int, col: Int)(inv: Boolean)(exp1P: Expression, exp2P: Expression)
                       (fun: (Double, Double) => Double):
          (Double, Double, Double, Double) = {
        val exp1 = if (!inv) exp1P else exp2P
        val exp2 = if (!inv) exp2P else exp1P
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return applyFunToRGB(inv)(exp1Res, exp2Res, fun)
      }

      val applyFunCurryImage = applyFunCurry(image, row, col)_
      val applyFunNormal = applyFunCurryImage(false)
      val applyFunInv = applyFunCurryImage(true)

      var value = (0.0, 0.0, 0.0, 0.0)
      this match {
      case Var("_this") => return image.getRGBADouble(row, col)
      case Num(number) => return (number, number, number, image.getA(row, col))
      case ColorExpression(color) =>
        return (color.getRed / Image.ComponentValues, color.getGreen / Image.ComponentValues,
                color.getBlue / Image.ComponentValues, color.getAlpha / Image.ComponentValues)
      case OperationAdd(exp1, exp2) =>
        return applyFunNormal(exp1, exp2)(Singleton.operationAdd.func)
      case OperationMultiply(exp1, exp2) =>
        return applyFunNormal(exp1, exp2)(Singleton.operationMultiply.func)
      case OperationSub(exp1, exp2) =>
        return applyFunNormal(exp1, exp2)(Singleton.operationSub.func)
      case OperationInvSub(exp1, exp2) =>
        return applyFunInv(exp1, exp2)(Singleton.operationSub.func)
      case OperationDiv(exp1, exp2) =>
        return applyFunNormal(exp1, exp2)(Singleton.operationDiv.func)
      case OperationInvDiv(exp1, exp2) =>
        return applyFunInv(exp1, exp2)(Singleton.operationDiv.func)
      case OperationSet(exp) =>
        val expRes = exp.calculate(image, row, col)
        return expRes
      case OperationPower(exp1, exp2) =>
        return applyFunNormal(exp1, exp2)(Singleton.operationPower.func)
      case OperationLog(exp1, exp2) =>
        return applyFunNormal(exp1, exp2)(Singleton.operationLog.func)
      case OperationAbs(exp) =>
          val expRes = exp.calculate(image, row, col)
          return (Math.abs(expRes._1), Math.abs(expRes._2), Math.abs(expRes._3), expRes._4)
      case OperationMin(exp1, exp2) =>
        return applyFunNormal(exp1, exp2)(Singleton.operationMin.func)
      case OperationMax(exp1, exp2) =>
        return applyFunNormal(exp1, exp2)(Singleton.operationMax.func)
      case (_) => println("Ostalo"); return (1, 1, 1, 1)
    }
  }
}

  case class Var(name: String) extends Expression {
    override def toString: String = name
  }

  case class OperationAdd(e1: Expression, e2: Expression) extends  Expression {
    override def toString: String = e1.toString + " + " + e2.toString
    def func(a: Double, b: Double) = a + b
  }

  case class OperationSub(e1: Expression, e2: Expression) extends  Expression {
    override def toString = e1.toString + " - " + e2.toString

    def func(a: Double, b: Double) = a - b
  }

  case class OperationInvSub(e1: Expression, e2: Expression) extends  Expression {
    override def toString = e1.toString + " - " + e2.toString
    def func(a: Double, b: Double) = a - b

  }

  case class OperationMultiply(e1: Expression, e2: Expression) extends Expression {
    override def toString = e1.toString + " * " + e2.toString

    def func(a: Double, b: Double) = a * b

  }

  case class OperationDiv(e1: Expression, e2: Expression) extends Expression {
    override def toString = e1.toString + " / " + e2.toString

    def func(a: Double, b: Double) = a / b
  }
  case class OperationInvDiv(e1: Expression, e2: Expression) extends Expression {
    override def toString = e1.toString + " / " + e2.toString

    def func(a: Double, b: Double) = a / b
  }

  case class OperationSet(e: Expression) extends Expression {
  }

  case class OperationPower(e1: Expression, e2: Expression) extends  Expression {
    override def toString = e1.toString + " ^ " + e2.toString

    def func(a: Double, b: Double) = Math.pow(a, b)
  }

  case class OperationLog(e1: Expression, e2: Expression) extends  Expression {
    override def toString = "Min(" + e1.toString + "," + e2.toString + ")"

    def func(a: Double, b: Double) = Math.log(a) / Math.log(b)
  }

  case class OperationAbs(e: Expression) extends  Expression {
    override def toString = "Abs(" + e.toString + ")"
    def func(a: Double, b: Double) = a / b

  }

  case class OperationMin(e1: Expression, e2: Expression) extends  Expression {
    override def toString = "Min(" + e1.toString + "," + e2.toString + ")"
    def func(a: Double, b: Double) = Math.min(a, b)
  }

  case class OperationMax(e1: Expression, e2: Expression) extends  Expression {
    override def toString = "Mаx(" + e1.toString + "," + e2.toString + ")"
    def func(a: Double, b: Double) = Math.max(a, b)
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


object Singleton {
  val operationAdd = OperationAdd(Num(1), Num(1))
  val operationMultiply = OperationMultiply(Num(1), Num(1))
  val operationSub = OperationSub(Num(1), Num(1))
  val operationDiv = OperationDiv(Num(1), Num(1))
  val operationPower = OperationPower(Num(1), Num(1))
  val operationLog = OperationLog(Num(1), Num(1))
  val operationMin = OperationMin(Num(1), Num(1))
  val operationMax = OperationMax(Num(1), Num(1))
}