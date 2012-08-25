package cz.cvut.fit.mi_paa.janecjak.sat

import java.io.{FileOutputStream, PrintWriter, File}


object SAT {

  def main(args: Array[String]) {
    for (file <- new File("satdata").listFiles() if file.isFile if file.getName.endsWith(".wcnf")) {
      println(file.getName)
      implicit val w = new PrintWriter(new FileOutputStream("./satresults/results.sol", true))

      val formula = Reader.read(file);

      // DEFAULTS
      //      val alpha = 0.5f
      //      val clauseBonus = 250
      //      val satisficationBonus = 700
      //      val startTemp = 5000
      //      val endTemp = 5
      //      val coolingFactor = 0.9
      //      val equilibrium = 2500

      // 'BEST'
      val alpha = 0.6f
      val clauseBonus = 450
      val satisficationBonus = 1000
      val startTemp = 7000
      val endTemp = 15
      val coolingFactor = 0.95
      val equilibrium = 2500

      w.print(file.getName)
      formula.clauseBonus = clauseBonus
      formula.satisfactionBonus = satisficationBonus
      run(formula, alpha, startTemp, endTemp, coolingFactor, equilibrium)

      //      val alphas = Array(0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f, 1.0f)
      //      w.println("alpha\tavgWeight\tbestWeight\tpercentOfSuccess\tavgTime\tbestTime")
      //      for (param <- alphas) {
      //        w.print(param)
      //        run(formula, param, startTemp, endTemp, coolingFactor, equilibrium)
      //      }
      //      w.println


      //      val clauseBonuses = Array(50, 100, 200, 300, 400, 500, 600)
      //      w.println("alpha\tavgWeight\tbestWeight\tpercentOfSuccess\tavgTime\tbestTime")
      //      for (param <- clauseBonuses) {
      //        w.print(param)
      //        formula.clauseBonus = param
      //        run(formula, alpha, startTemp, endTemp, coolingFactor, equilibrium)
      //      }
      //      w.println
      //
      //      val satisfactionBonuses = Array(200, 400, 600, 800, 1000, 1500)
      //      w.println("alpha\tavgWeight\tbestWeight\tpercentOfSuccess\tavgTime\tbestTime")
      //      for (param <- satisfactionBonuses) {
      //        w.print(param)
      //        formula.satisfactionBonus = param
      //        run(formula, alpha, startTemp, endTemp, coolingFactor, equilibrium)
      //      }
      //      w.println
      //
      //      val startTemps = Array(1000, 3000, 5000, 7000, 9000, 11000)
      //      w.println("alpha\tavgWeight\tbestWeight\tpercentOfSuccess\tavgTime\tbestTime")
      //      for (param <- startTemps) {
      //        w.print(param)
      //        run(formula, alpha, param, endTemp, coolingFactor, equilibrium)
      //      }
      //      w.println
      //
      //      val endTemps = Array(3, 5, 10, 20, 40, 80, 200)
      //      w.println("alpha\tavgWeight\tbestWeight\tpercentOfSuccess\tavgTime\tbestTime")
      //      for (param <- endTemps) {
      //        w.print(param)
      //        run(formula, alpha, startTemp, param, coolingFactor, equilibrium)
      //      }
      //      w.println
      //
      //      val coolingFactors = Array(0.2f, 0.5f, 0.8f, 0.9f, 0.95f, 0.98f)
      //      w.println("alpha\tavgWeight\tbestWeight\tpercentOfSuccess\tavgTime\tbestTime")
      //      for (param <- coolingFactors) {
      //        w.print(param)
      //        run(formula, alpha, startTemp, endTemp, param, equilibrium)
      //      }
      //      w.println
      //
      //      val equilibriums = Array(500, 1000, 1500, 2000, 3000, 5000, 7000)
      //      w.println("alpha\tavgWeight\tbestWeight\tpercentOfSuccess\tavgTime\tbestTime")
      //      for (param <- equilibriums) {
      //        w.print(param)
      //        run(formula, alpha, startTemp, endTemp, coolingFactor, param)
      //      }
      //      w.println

      w.close
    }
  }

  def run(formula: Formula, alpha: Float, startTemp: Int, endTemp: Int, coolingFactor: Double, equilibrium: Int)(implicit w: PrintWriter) = {
    var avgWeight = 0f
    var bestWeight = 0f
    var avgTime = 0.0
    var bestTime = java.lang.Long.MAX_VALUE
    var percentOfSuccess = 0f

    val cycles = 3
    for (i <- 0 until cycles) {
      val startTime = System.nanoTime
      val solved = new SAT(formula, alpha, startTemp, endTemp, coolingFactor, equilibrium).solve
      val time = System.nanoTime - startTime

      if (solved != null) {
        val w = formula.weight(solved)
        avgWeight += w
        avgTime += time
        if (w > bestWeight) {
          bestWeight = w
        }

        if (time < bestTime) {
          bestTime = time
        }

        if (formula.eval(solved)) {
          percentOfSuccess += 1
        }
      }
    }

    avgWeight /= cycles
    avgTime /= cycles
    percentOfSuccess /= cycles

    w.println("\t" + avgWeight + "\t" + bestWeight + "\t" + percentOfSuccess + "\t" + avgTime / (1000 * 1000) + "\t" + bestTime.toDouble / (1000 * 1000))
    w.flush()
  }

}

class SAT(private val formula: Formula,
          private val alpha: Float,
          private val startTemp: Double,
          private val finalTemp: Double,
          private val tempCoolingCoeficient: Double,
          private val equilibrium: Int) {

  private val n = formula.w.length
  private val random = new java.util.Random()

  def solve = {
    simulatedAnnealing
  }

  private def simulatedAnnealing = {
    var temp = startTemp
    var state = generateRandom
    var best: Array[Boolean] = null
    var stateFitness = formula.fitness(state, alpha)
    var bestFitness = 0f

    while (temp > finalTemp) {
      var equil = equilibrium

      while (equil > 0) {
        val newState = generateNew(state, bestFitness, temp);
        val newStateFitness = formula.fitness(newState, alpha)

        if (newStateFitness > stateFitness && formula.eval(newState)) {
          best = newState
          bestFitness = newStateFitness
        }

        equil -= 1

        state = newState
        stateFitness = newStateFitness
      }

      // cool down
      temp = temp * tempCoolingCoeficient
    }

    best
  }

  private def generateNew(state: Array[Boolean], bestFitness: Float, temp: Double): Array[Boolean] = {
    val rand = random.nextInt(n)

    val newState = state.updated(rand, !state(rand))
    val newStateFitness = formula.fitness(newState, alpha);
    val stateFitness = formula.fitness(state, alpha)

    if (newStateFitness > bestFitness) {
      newState
    }
    else {
      val diff = newStateFitness - stateFitness

      if (random.nextDouble() < java.lang.Math.pow(java.lang.Math.E, -diff / temp)) {
        newState
      }
      else {
        state
      }
    }
  }

  private def generateRandom: Array[Boolean] = {
    var state: Array[Boolean] = Array.fill(n)(false)

    for (i <- 0 until n) {
      val rand = random.nextDouble()
      if (rand > 0.5) {
        state.update(i, true)
      }
    }

    state
  }

}
