package rs.ac.bg.etf.zd173013m.logic.operation

import java.util.function.DoubleUnaryOperator

import rs.ac.bg.etf.zd173013m.logic.image.Image
import rs.ac.bg.etf.zd173013m.logic.operation.Operations._

import scala.swing.Color

object Operations {
  sealed trait Expression {
    var evaluated: Boolean = false

    def copyOverride: Expression

    protected def funcCalculateRGBAExpr(image: Image, row: Int, col: Int)(e: Expression,
                                                            fun: ((Double, Double, Double, Double)) => (Double, Double, Double, Double)):
                        (Double, Double, Double, Double) = {
      val expRes: (Double, Double, Double, Double) = e.calculate(image, row, col)
      return fun(expRes)
    }

    def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double)

    def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double)

    def rgbaapplyTorgb(fun: (Double) => Double) =
      (value: (Double, Double, Double, Double)) => {
        (fun(value._1), fun(value._2), fun(value._3), value._4)
      }

    private def calculate(image: Image, row: Int, col: Int): (Double, Double, Double, Double) =
    {
      def applyFunOnlyToRGB2(val1: (Double, Double, Double), val2: (Double, Double, Double),
                                       fun: (Double, Double)=> Double): (Double, Double, Double) =
      {
        return (fun(val1._1, val2._1), fun(val1._2, val2._2), fun(val1._3, val2._3))
      }

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

        case operationBin @ (OperationAdd(_, _) | OperationMultiply(_, _)
          | OperationSub(_, _) | OperationDiv(_, _) | OperationPower(_, _) |
          OperationLog(_, _) | OperationMin(_, _) | OperationMax(_, _) |
          OperationSet(_) | OperationAbs(_) | OperationInvertColor(_) | OperationGrayScale(_)
          | OperationInvDiv(_, _) | OperationInvSub(_, _) | OperationComposite(_, _, _)) =>
          return operationBin.funcCalculateRGBA(image, row, col)
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
        case OperationSequence(exp, _, listOperations) =>
          {
            if (!exp.evaluated)
            {
              evaluateAllSelected(image, exp)
            }
            for (it <- listOperations)
            {
            if (!it.evaluated)
              {
                evaluateAllSelected(image, it)
              }
            }
            return image.getRGBADouble(row, col)
          }
        case (_) => println("Ostalo"); return (1, 1, 1, 1)
    }
   }

    private def evaluateAllSelected(image: Image, exp: Expression): Unit = {
      val tmpBuffer = image.pixelsComponents.clone()
      for (row <- 0 until image.icon.getIconHeight; col <- 0 until image.icon.getIconWidth
      if image.isSelected(row, col)) {
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

    override def copyOverride: Expression = this.copy()

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = ???
    def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = ???
  }
  case class Num(value: Double) extends  Expression {
    override def toString = value.toString
    override def copyOverride: Expression = this.copy()

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = ???
    def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = ???
  }

  case class ColorExpression(value: Color) extends Expression {
    override def copyOverride: Expression = this.copy()

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = ???
    def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = ???

  }

  case class OperationAdd(val e1: Expression, val e2: Num) extends  Expression {
    override def toString: String = e1.toString + " + " + e2.toString

    override def copyOverride: Expression = this.copy()

    def func(a: Double, b: Double) = a + b

    def func1(a: Double) : Double = func(a, e2.value)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationSub(e1: Expression, e2: Num) extends  Expression {
    override def toString = e1.toString + " - " + e2.toString
    override def copyOverride: Expression = this.copy()
    def func(a: Double, b: Double) = a - b
    def func1(a: Double) : Double = func(a, e2.value)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationInvSub(e1: Expression, e2: Num) extends  Expression {
    override def toString = e1.toString + " - " + e2.toString
    override def copyOverride: Expression = this.copy()
    def func(a: Double, b: Double) = b - a
    def func1(a: Double) : Double = func(a, e2.value)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationMultiply(e1: Expression, e2: Num) extends Expression {
    override def toString = e1.toString + " * " + e2.toString

    override def copyOverride: Expression = this.copy()
    def func(a: Double, b: Double) = a * b
    def func1(a: Double) : Double = func(a, e2.value)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationDiv(e1: Expression, e2: Num) extends Expression {
    override def toString = e1.toString + " / " + e2.toString

    override def copyOverride: Expression = this.copy()
    def func(a: Double, b: Double) = a / b
    def func1(a: Double) : Double = func(a, e2.value)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }
  case class OperationInvDiv(e1: Expression, e2: Num) extends Expression {
    override def toString = e1.toString + " / " + e2.toString

    override def copyOverride: Expression = this.copy()
    def func(a: Double, b: Double) = b / a
    def func1(a: Double) : Double = func(a, e2.value)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationSet(e: Expression) extends Expression {
    override def copyOverride: Expression = this.copy()
    def func(a: Double, b: Double) = a
    def func1(a: Double) : Double = func(a, 0)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationPower(e1: Expression, e2: Num) extends  Expression {
    override def toString = e1.toString + " ^ " + e2.toString

    override def copyOverride: Expression = this.copy()
    def func(a: Double, b: Double) = Math.pow(a, b)
    def func1(a: Double) : Double = func(a, 0)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationLog(e1: Expression, e2: Num) extends  Expression {
    override def toString = "Log(" + e1.toString + "," + e2.toString + ")"

    override def copyOverride: Expression = this.copy()

    def func(a: Double, b: Double) = Math.log(a) / Math.log(b)

    def func1(a: Double) : Double = func(a, e2.value)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationAbs(e: Expression) extends  Expression {
    override def toString = "Abs(" + e.toString + ")"
    override def copyOverride: Expression = this.copy()

    def func1(a: Double) : Double = Math.abs(a)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationMin(e1: Expression, e2: Num) extends  Expression {
    override def toString = "Min(" + e1.toString + "," + e2.toString + ")"
    override def copyOverride: Expression = this.copy()

    def func(a: Double, b: Double) = Math.min(a, b)

    def func1(a: Double) : Double = func(a, e2.value)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationMax(e1: Expression, e2: Num) extends  Expression {
    override def toString = "Mаx(" + e1.toString + "," + e2.toString + ")"
    override def copyOverride: Expression = this.copy()

    def func(a: Double, b: Double) = Math.max(a, b)

    def func1(a: Double) : Double = func(a, e2.value)

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e1, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationInvertColor(e: Expression) extends Expression {
    override def copyOverride: Expression = this.copy()
    def func1(a: Double) : Double = 1 - a

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e, rgbaapplyTorgb(func1))
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = rgbaapplyTorgb(func1)
  }

  case class OperationGrayScale(e: Expression) extends Expression {
    override def copyOverride: Expression = this.copy()

    def grayScale(rgba: (Double, Double, Double, Double)) :(Double, Double, Double, Double)= {
      val avg = (rgba._1 + rgba._2 + rgba._3) / 3.0
      return (avg,avg, avg, rgba._4)
    }

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = return funcCalculateRGBAExpr(image, row, col)(e, grayScale)
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = grayScale
  }

  case class OperationComposite(e: Expression, name: String, list: List[Expression]) extends Expression
  {
    override def toString:String = {
      var output: String = "list_" + name
      list.foreach(output += " " + _)
      return output
    }

    var func1 = list.head.getFun1

    private var isFirst = true
    for (it <- list) {
      it match {
        case operationBin @ (OperationAdd(_, _) | OperationMultiply(_, _)
                             | OperationSub(_, _) | OperationDiv(_, _) | OperationPower(_, _) |
                             OperationLog(_, _) | OperationMin(_, _) | OperationMax(_, _) |
                             OperationSet(_) | OperationAbs(_) | OperationInvertColor(_) | OperationGrayScale(_)
                             | OperationInvDiv(_, _) | OperationInvSub(_, _) | OperationComposite(_, _, _)) if !isFirst =>
          func1 = func1.andThen(it.getFun1)
        case _ => println("SOmething is really bad")
      }
      isFirst = false
    }


    override def copyOverride: Expression = this.copy()
    override def funcCalculateRGBA(image: Image, row: Int, col: Int): (Double, Double, Double, Double) =
      return funcCalculateRGBAExpr(image, row, col)(e, func1)

    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = func1
  }

  case class OperationMedian(e1: Expression, n: Int) extends Expression {
    override def copyOverride: Expression = this.copy()

    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = ???

    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = ???
  }

  case class OperationPond(e: Expression, matrix: Array[Array[(Double, Double, Double)]]) extends Expression {
    override def copyOverride: Expression = this.copy()
    override def funcCalculateRGBA(image: Image, row: Int, col: Int):
    (Double, Double, Double, Double) = ???
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = ???
  }

  case class OperationSequence(e: Expression, name: String, list: List[Expression]) extends Expression
  {
    override def toString:String = {
      var output: String = "list_" + name
      list.foreach(output += " " + _)
      return output
    }
    override def copyOverride: Expression = this.copy()
    override def funcCalculateRGBA(image: Image, row: Int, col: Int): (Double, Double, Double, Double) = ???
    override def getFun1: ((Double, Double, Double, Double)) => (Double, Double, Double, Double) = ???
  }


}

object Singleton {
  val operationAdd = OperationAdd(Num(1), Num(1))
  val operationMultiply = OperationMultiply(Num(1), Num(1))
  val operationDiv = OperationDiv(Num(1), Num(1))
}