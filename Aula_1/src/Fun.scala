package Aula_1

object Fun {
  //singleton object
    def func1(x: Double, y: Int) = x + (70 * y)

    def ex(a: Double) = 50 * a

    //
    def method1 (x:(Int,Int), y:(Int,Int)): (Int, Int) =
      ((x._1 + x._2), (y._1 * y._2) )

    def method2 (x: Int, y:Int, z:Int): (Int, Int) =
    {
      (largestBetween3(x,y,z), middleBetween3 (x,y,z))
    }

  def largestBetween3(a: Int, b: Int, c: Int): Int = {
    if (a > b)
      if (c > a) c else a
    else
      if (c > b) c else b
  }

  def smalestBetween3(a: Int, b: Int, c: Int): Int = {
    if (a < b)
      if (c < a) c else a
    else if (c < b) c else b
  }

  def largestBetween2(a: Int, b: Int): Int = {
    if (a > b) a else b
  }

  def smalestBetween2(a: Int, b: Int): Int = {
    if (a > b) b else a
  }

  def middleBetween3(a: Int, b: Int, c: Int): Int = {
    smalestBetween2(largestBetween2(a, b), largestBetween2(smalestBetween2(a, b), c))
  }

    def min (x:Int, y:Int): (Int) = if (x < y) x else y

    def method3 (x:(Int, Int, Int)): (Int,Int,Int) =
      ( min(min(x._1,x._2), x._3),(method2(x._1,x._2,x._3))._2,(method2(x._1,x._2,x._3))._1 )

    def method4 (x:Int,y:Int, z:Int): (Boolean) =
      if(x+y > z) true
      else if (x+z > y) true
      else if (y+z > x) true
      else false

    def abrev (x:String): (String) =
      x.split("").head ++ "" ++ x.split("").last

    ///////

    def factorial (x:Int): Int =
      if (x==0) 1
      else x*factorial(x-1)

    def expon (x:Int, y:Int): Int =
      if(y==0) 1
      else x*expon(x,y-1)

    def listing (x:List[Int]): (Int, Int) = (x.head, x.last)

    def listLength (x:List[Int]): (List[Int], Int) =
      (x, x.length)

    def average (x:List[Double]): (Double) =
      sum(x)/x.length

    def sum (x:List[Double]): Double = {
      if(x.isEmpty) 0
      else x.head + sum(x.tail)
    }
}
