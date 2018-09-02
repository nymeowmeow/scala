package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      Math.pow(b(), 2) - 4*(a()*c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val minusB = Signal(-1*b())
    val denominator = Signal(2*a())
    val sqrtDelta = Signal(math.sqrt(delta()))
    Signal {
      if (delta() < 0) Set[Double]()
      else {
        Set(
          (minusB() + sqrtDelta())/denominator(),
          (minusB() - sqrtDelta())/denominator()
        )
      }
    }
  }
}
