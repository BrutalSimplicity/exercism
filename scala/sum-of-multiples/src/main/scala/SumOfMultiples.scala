
object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = {
    def isMultiple(factors: Set[Int], n: Int): Boolean = factors.headOption match {
      case None => false
      case Some(factor) if n % factor == 0 => true
      case _ => isMultiple(factors.tail, n)
    }

    def localSum(acc: Int, limit: Int): Int =
      if (limit == 0)
        acc
      else if (isMultiple(factors, limit))
        localSum(acc + limit, limit - 1)
      else
        localSum(acc, limit - 1)

    localSum(0, limit - 1)
  }

  def sum2(factors: Set[Int], limit: Int): Int = {
    var sum = 0
    for (i <- 1 until limit if factors.exists(i % _ == 0)) {
      sum += i
    }
    sum
  }

  def sum3(factors: Set[Int], limit:Int): Int = {
    (1 until limit).filter(n => factors.exists(n % _ == 0)).sum
  }
}

