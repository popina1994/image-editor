package rs.ac.bg.etf.zd173013m

class Test
{
  def nesto = "Nesto"
}

object Test {
  def main(args: Array[String]) {
    var sum: Int = 0
    for  (arg <- args)
    {
      println(arg)
      sum =  sum + arg.toInt
    }
    println(f"Suma je: $sum")
  }
}
