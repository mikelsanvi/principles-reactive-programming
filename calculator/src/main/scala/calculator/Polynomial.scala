package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal({
      b() * b() - 4 * a() * c()
    })
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      if(delta() < 0 )
        Set()
      else{
        val deltaSqrt = Math.sqrt(delta())
        Set((-b() + deltaSqrt)/(2 *a()),(-b() - deltaSqrt)/(2 *a()) )
      }
    })
  }
}
