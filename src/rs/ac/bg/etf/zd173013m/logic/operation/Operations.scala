package rs.ac.bg.etf.zd173013m.logic.operation

import rs.ac.bg.etf.zd173013m.logic.image.Image
import rs.ac.bg.etf.zd173013m.logic.operation.Operations._

import scala.swing.Color

object Operations {
  sealed trait Expression {
    var evaluated: Boolean = false

    // TODO: Replace with partially applied function.
    private def calculate(image: Image, row: Int, col: Int): (Double, Double, Double, Double) =
    {

      def applyFunToOnlyRGB1(value: (Double, Double, Double, Double),
                         fun: (Double)=> Double): (Double, Double, Double) =
        return (fun(value._1), fun(value._2), fun(value._3))

      def applyFunToRGB1(value: (Double, Double, Double, Double),
                         fun: (Double)=> Double): (Double, Double, Double, Double) =
        return (fun(value._1), fun(value._2), fun(value._3), value._4)

      def applyFunToRGB2(inv: Boolean)(val1: (Double, Double, Double, Double), val2: (Double, Double, Double, Double),
                        fun: (Double, Double)=> Double): (Double, Double, Double, Double) =
      {
        val alpha = if (!inv) val1._4 else val2._4
        return (fun(val1._1, val2._1), fun(val1._2, val2._2), fun(val1._3, val2._3), alpha)
      }

      def applyFunOnlyToRGB2(val1: (Double, Double, Double), val2: (Double, Double, Double),
                                       fun: (Double, Double)=> Double): (Double, Double, Double) =
      {
        return (fun(val1._1, val2._1), fun(val1._2, val2._2), fun(val1._3, val2._3))
      }

      def applyFunCurry1(image: Image, row: Int, col: Int)(exp: Expression)
                        (fun: (Double) => Double):(Double, Double, Double, Double) = {
        val expRes = exp.calculate(image, row, col)
        return applyFunToRGB1(expRes, fun)
      }

      def applyFunCurry2(image: Image, row: Int, col: Int)(inv: Boolean)(exp1P: Expression, exp2P: Expression)
                       (fun: (Double, Double) => Double):
          (Double, Double, Double, Double) = {
        val exp1 = if (!inv) exp1P else exp2P
        val exp2 = if (!inv) exp2P else exp1P
        val exp1Res = exp1.calculate(image, row, col)
        val exp2Res = exp2.calculate(image, row, col)
        return applyFunToRGB2(inv)(exp1Res, exp2Res, fun)
      }

      val applyFunCurryImage2 = applyFunCurry2(image, row, col)_
      val applyFunNormal2 = applyFunCurryImage2(false)
      val applyFunInv2 = applyFunCurryImage2(true)
      val applyFun1 = applyFunCurry1(image, row, col)_
      def findMedian(n: Int):(Double, Double, Double, Double) = {
        var sum = (0.0, 0.0, 0.0)
        var count = 0
        for (rowIdx <- row - n to row + n; colIdx <- col - n to col +n)
          image.getRGBADoubleCheck(rowIdx, colIdx) match {
            case None =>
            case Some(rgba) =>
              count += 1
              sum = applyFunOnlyToRGB2((rgba._1, rgba._2, rgba._3), sum, Singleton.operationAdd.func)
          }
        return (sum._1 / count, sum._2 / count, sum._3 / count, image.getRGBADouble(row, col)._4)
      }

      def grayScale(exp: Expression) :(Double, Double, Double, Double)= {
        val value = exp.calculate(image, row, col)
        val avg = (value._1 + value._2 + value._3) / 3.0
        return (avg,avg, avg, value._4)
      }

      def pond(expression: Expression, matrix: Array[Array[(Double, Double, Double)]]):(Double, Double, Double, Double)= {
        val n = matrix.length / 2
        var sum = (0.0, 0.0, 0.0)
        var count = 0
        val startRowIdx = row - n
        val startColIdx = col - n
        for (rowIdx <- row - n to row + n; colIdx <- col - n to col +n)
          image.getRGBADoubleCheck(rowIdx, colIdx) match {
            case None =>
            case Some(rgba) =>
              count += 1
              val mulVal = applyFunOnlyToRGB2((rgba._1, rgba._2, rgba._3),
                matrix(rowIdx - startRowIdx)(colIdx - startColIdx),
                Singleton.operationMultiply.func)
              sum = applyFunOnlyToRGB2(sum, mulVal, Singleton.operationAdd.func)
          }
        return (sum._1 / count, sum._2 / count, sum._3 / count, image.getRGBADouble(row, col)._4)

      }

      if (evaluated) return image.getRGBADouble(row, col)
      this match {
        case Var("_this") => return image.getRGBADouble(row, col)
        case Num(number) => return (number, number, number, image.getADouble(row, col))
        case ColorExpression(color) =>
          return (color.getRed / Image.ComponentValues, color.getGreen / Image.ComponentValues,
                  color.getBlue / Image.ComponentValues, color.getAlpha / Image.ComponentValues)
        case OperationAdd(exp1, exp2) =>
          return applyFunNormal2(exp1, exp2)(Singleton.operationAdd.func)
        case OperationMultiply(exp1, exp2) =>
          return applyFunNormal2(exp1, exp2)(Singleton.operationMultiply.func)
        case OperationSub(exp1, exp2) =>
          return applyFunNormal2(exp1, exp2)(Singleton.operationSub.func)
        case OperationInvSub(exp1, exp2) =>
          return applyFunInv2(exp1, exp2)(Singleton.operationSub.func)
        case OperationDiv(exp1, exp2) =>
          return applyFunNormal2(exp1, exp2)(Singleton.operationDiv.func)
        case OperationInvDiv(exp1, exp2) =>
          return applyFunInv2(exp1, exp2)(Singleton.operationDiv.func)
        case OperationSet(exp) =>
          return applyFun1(exp)(Singleton.operationSet.func)
        case OperationPower(exp1, exp2) =>
          return applyFunNormal2(exp1, exp2)(Singleton.operationPower.func)
        case OperationLog(exp1, exp2) =>
          return applyFunNormal2(exp1, exp2)(Singleton.operationLog.func)
        case OperationAbs(exp) =>
            return applyFun1(exp)(Singleton.operationAbs.func)
        case OperationMin(exp1, exp2) =>
          return applyFunNormal2(exp1, exp2)(Singleton.operationMin.func)
        case OperationMax(exp1, exp2) =>
          return applyFunNormal2(exp1, exp2)(Singleton.operationMax.func)
        case OperationInvertColor(exp) =>
          return applyFun1(exp)(Singleton.operationInvertColor.func)
        case OperationGrayScale(exp) =>
          return grayScale(exp)
        case OperationMedian(exp, n) =>
          if (!exp.evaluated)
          {
            evaluateAllSelected(image, exp)
          }
          return findMedian(n)
        case OperationPond(exp, mat) =>
          if (!exp.evaluated)
            {
              evaluateAllSelected(image, exp)
            }
          return pond(exp, mat)
        case (_) => println("Ostalo"); return (1, 1, 1, 1)
    }
   }

    private def evaluateAllSelected(image: Image, exp: Expression): Unit = {
      val tmpBuffer = image.pixelsComponents.clone()
      for (row <- 0 until image.icon.getIconHeight; col <- 0 until image.icon.getIconWidth) {
        val expEval = exp.calculate(image, row, col)
        if ((row == 0) && (col == 0))
          println(expEval)
        image.setRGBADouble(row, col, expEval, tmpBuffer)
      }
      image.pixelsComponents = tmpBuffer
      exp.evaluated = true
    }

    def calculateSelectedPixels(image: Image): Unit = {
      evaluateAllSelected(image, this)
    }
  }

  case class Var(name: String) extends Expression {
    override def toString: String = name
  }
  case class Num(value: Double) extends  Expression {
    override def toString = value.toString
  }

  case class ColorExpression(value: Color) extends Expression {

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
    def func(a: Double) = a
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
    def func(a: Double) = Math.abs(a)

  }

  case class OperationMin(e1: Expression, e2: Expression) extends  Expression {
    override def toString = "Min(" + e1.toString + "," + e2.toString + ")"
    def func(a: Double, b: Double) = Math.min(a, b)
  }

  case class OperationMax(e1: Expression, e2: Expression) extends  Expression {
    override def toString = "Mаx(" + e1.toString + "," + e2.toString + ")"
    def func(a: Double, b: Double) = Math.max(a, b)
  }

  case class OperationInvertColor(e: Expression) extends Expression {
    def func(a: Double) = 1 - a
  }

  case class OperationGrayScale(e: Expression) extends Expression {
  }

  case class OperationMedian(e1: Expression, n: Int) extends Expression {
  }

  case class OperationPond(e: Expression, matrix: Array[Array[(Double, Double, Double)]]) extends Expression {

  }

  case class OperationSequence(name: String, list: List[Expression]) extends Expression
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
  val operationAbs = OperationAbs(Num(1))
  val operationSet = OperationSet(Num(1))
  val operationInvertColor = OperationInvertColor(Num(1))
}