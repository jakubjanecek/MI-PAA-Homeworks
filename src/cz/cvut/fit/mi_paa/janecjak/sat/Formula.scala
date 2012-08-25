package cz.cvut.fit.mi_paa.janecjak.sat

class Formula(val clauses: List[Clause], val w: Array[Int]) {

  var clauseBonus = 0
  var satisfactionBonus = 0

  private val weights = normalize(w)

  def eval(input: Array[Boolean]): Boolean = {
    if (clauses.length > 1) {
      //      clauses.map(_.eval(input)).reduceLeft(_ && _)

      clauses.find(_.eval(input) == false) match {
        case Some(_) => false
        case None => true
      }
    }
    else {
      clauses(0).eval(input)
    }
  }

  def weight(input: Array[Boolean]) = {
    var weightSum = 0f
    for (i <- input.zipWithIndex) {
      if (i._1 == true) {
        weightSum += w(i._2)
      }
    }

    weightSum
  }

  /**
   * FITNESS FUNCTION
   *
   *          satisfiedClauses
   * alpha * ------------------- + (1 - alpha) * sumOfNormalizedWeightsOfInputVars
   *           numOfClauses
   *
   * The bigger the alpha, the more important is number of satisfied clauses, and the smaller the alpha,
   * the more important are normalized weights.
   */
  def fitness(input: Array[Boolean], alpha: Float): Float = {
    var fit = 0f

    var satisfied = 0

    if (clauses.length > 1) {
      for (clause <- clauses) {
        if (clause.eval(input)) {
          satisfied += 1
          fit += clauseBonus
        }
      }
    }
    else {
      if (clauses(0).eval(input)) {
        satisfied += 1
        fit += clauseBonus
      }
    }

    if (satisfied == clauses.length) {
      fit += satisfactionBonus
    }

    var weightSum = 0f
    for (i <- input.zipWithIndex) {
      if (i._1 == true) {
        weightSum += weights(i._2)
      }
    }

    //    (alpha * (satisfied.toFloat / clauses.length)) + ((1 - alpha) * weightSum)

    fit + weightSum
  }

  override def toString = {
    var str = clauses.mkString(".")
    str += "\n"
    for (i <- 0 until weights.length) {
      str += "x" + (i + 1) + " = " + weights(i) + ", "
    }
    str
  }

  private def normalize(denormalized: Array[Int]): Array[Float] = {
    val norm = denormalized.max

    denormalized.map(x => x.toFloat / norm)
  }

}
