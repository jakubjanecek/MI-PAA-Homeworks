package cz.cvut.fit.mi_paa.janecjak.sat

import collection.mutable.HashMap


class Clause(val variables: Array[Symbol]) {

  private val cache = new HashMap[Array[Boolean], Boolean]()

  def eval(input: Array[Boolean]): Boolean = {
    if (cache.contains(input)) {
      val r = cache.get(input).get
      if (cache.size > 50000) {
        cache.clear()
      }

      return r
    }

    val varsFilteredWithIndex = variables.zipWithIndex.filter(x => x._1 == 'present || x._1 == 'presentNegated)
    if (varsFilteredWithIndex.length > 1) {
      varsFilteredWithIndex.map(x => evalVariable(x._1, input(x._2))).reduceLeft(_ || _)

      val res = varsFilteredWithIndex.find(x => evalVariable(x._1, input(x._2) == true)) match {
        case Some(_) => true
        case None => false
      }

      cache.put(input, res)

      res
    }
    else {
      val res = evalVariable(varsFilteredWithIndex(0)._1, input(varsFilteredWithIndex(0)._2))

      cache.put(input, res)

      res
    }
  }

  private def evalVariable(s: Symbol, i: Boolean): Boolean = s match {
    case 'present => i
    case 'presentNegated => !i
  }

  override def toString = {
    var str = ""
    for (i <- 0 until variables.length) {
      if (variables(i) != 'notPresent) {
        if (variables(i) == 'present) {
          str += "x" + (i + 1) + " + "
        }
        else {
          str += "'x" + (i + 1) + " + "
        }
      }
    }
    "(" + str.stripSuffix(" + ") + ")"
  }

}
