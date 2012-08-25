package cz.cvut.fit.mi_paa.janecjak

import io.Source
import java.io.{FileOutputStream, PrintWriter, File}

object KnapsackSimulatedAnnealing {

  def main(args: Array[String]): Unit = {
    for (file <- new File("data").listFiles() if file.isFile if file.getName.endsWith("inst.dat")) {
      val w = new PrintWriter(new FileOutputStream("./results/" + file.getName + ".sol.dat", true))

      val startTemp = 15000
      val endTemp = 1
      val coolFactor = 0.85
      val equilibrium = 3000

      var avgError = 0D
      var avgTime = 0L
      var count = 0
      for (line <- Source.fromFile(file.getAbsolutePath).getLines) {
        val Array(id, n, m, rest@_*) = line.split(" ")
        val groupedItems = rest.grouped(2)

        val items = groupedItems.toArray.map(x => (x.head.toInt, x.tail.head.toInt))

        val startD = System.nanoTime
        val resultD = new Knapsack(n.toInt, m.toInt, items).dynamicProgramming
        val timeD = System.nanoTime - startD

        val startSA = System.nanoTime()
        val resultSA = new KnapsackSimulatedAnnealing(n.toInt, m.toInt, items, startTemp, endTemp, coolFactor, equilibrium).solve
        val timeSA = System.nanoTime() - startSA

        val error = (resultD._2.toDouble - resultSA._2.toDouble) / resultD._2.toDouble

        avgError += error
        avgTime += timeSA
        count += 1

        //        w.println(id + "\t" + timeD + "\t" + timeSA + "\t" + error)
      }

      avgError = avgError / count
      avgTime = avgTime / count

      w.println(startTemp + "-" + endTemp + ";" + coolFactor + ";" + equilibrium + "\t" + avgTime.toDouble / (1000 * 1000 * 1000) + "\t" + avgError * 100)

      w.close
    }
  }
}

class KnapsackSimulatedAnnealing(private val count: Int,
                                 private val maxWeight: Int,
                                 private val items: Array[Tuple2[Int, Int]],
                                 private val startTemp: Double,
                                 private val finalTemp: Double,
                                 private val tempCoolingCoeficient: Double,
                                 private val equilibrium: Int) {

  def solve = {
    val best = simulatedAnnealing
    val bestValue = computeItem(best)

    (bestValue._1, bestValue._2, best)
  }

  private def simulatedAnnealing = {
    var temp = startTemp
    var state, best = generateRandom
    var stateValue, bestValue = computeItem(state)

    while (temp > finalTemp) {
      var equil = equilibrium

      while (equil > 0) {
        val newState = generateNew(state, bestValue, temp);
        val newStateValue = computeItem(state)

        if (newStateValue._2 > bestValue._2) {
          best = newState
          bestValue = newStateValue
        }

        equil -= 1

        state = newState
        stateValue = newStateValue
      }

      // cool down
      temp = temp * tempCoolingCoeficient
    }

    best
  }

  private def generateNew(state: Seq[Int], bestValue: (Int, Int), temp: Double): Seq[Int] = {
    val random = new java.util.Random
    val rand = random.nextInt(count)
    val newVal = random.nextInt(2)

    val newState = state.updated(rand, newVal)

    if (computeItem(newState)._1 > maxWeight) {
      generateNew(state, bestValue, temp)
    }
    else {
      val newStateValue = computeItem(newState);
      val stateValue = computeItem(state)
      if (newStateValue._2 > bestValue._2) {
        newState
      }
      else {
        val diff = newStateValue._2 - stateValue._2

        if (random.nextDouble() < java.lang.Math.pow(java.lang.Math.E, -diff / temp)) {
          newState
        }
        else {
          state
        }
      }
    }
  }

  private def generateRandom: Seq[Int] = {
    var state: Seq[Int] = Array.fill(count)(0)

    for (i <- 0 until count) {
      val rand = java.lang.Math.random
      if (rand > 0.5) {
        state = state.updated(i, 1)
      }
    }

    if (computeItem(state)._1 > maxWeight) {
      generateRandom
    }
    else {
      state
    }

  }

  private def computeItem(configuration: Seq[Int]) = {
    var i = 0
    var weight = 0
    var price = 0
    for (item <- items) {
      if (configuration(i) == 1) {
        weight += item._1
        price += item._2
      }
      i += 1
    }
    (weight, price)
  }

}