package cz.cvut.fit.mi_paa.janecjak

import io.Source
import java.io.{PrintWriter, File}

object KnapsackExperiment {

  def main(args: Array[String]): Unit = {
    val results = collection.mutable.LinkedHashMap[String, Double]()
    new File("results").mkdir()
    val w = new PrintWriter("results/results.csv")
    w.println("file\tHEU\tHEUS\tBB\tDYN");
    for (file <- new File("data").listFiles() if file.isFile if file.getName.endsWith("inst.dat")) {
      var c = 0
      var diff = 0f
      var sum1 = 0L
      var sum2 = 0L
      var sum3 = 0L
      for (line <- Source.fromFile(file.getAbsolutePath).getLines) {
        val Array(id, n, m, rest@_*) = line.split(" ")
        val groupedItems = rest.grouped(2)
        val items = groupedItems.toArray.map(x => (x.head.toInt, x.tail.head.toInt))

        val result1 = new KnapsackExperiment(n.toInt, m.toInt, items).bruteForce
        val result2 = new KnapsackExperiment(n.toInt, m.toInt, items).priceWeightHeuristic

        diff += (result1._2.toFloat - result2._2.toFloat) / result1._2.toFloat
        sum3 += result2._4

        val result3 = new KnapsackExperiment(n.toInt, m.toInt, items).branchAndBound
        val result4 = new KnapsackExperiment(n.toInt, m.toInt, items).dynamicProgramming

        sum1 += result3._4
        sum2 += result4._4
        c += 1
      }
      w.println(file.getName + "\t" + (diff / c.toFloat) + "\t" + (sum3 / c) + "\t" + (sum1 / c) + "\t" + (sum2 / c));
      results += file.getName + " HEU " -> diff / c.toFloat
      results += file.getName + " HEUS" -> sum3 / c
      results += file.getName + " BB  " -> sum1 / c
      results += file.getName + " DYN " -> sum2 / c
    }
    println(results.mkString("\n"))
    w.close()
  }

}

class KnapsackExperiment(private val count: Int, private val maxWeight: Int, private val items: Array[Tuple2[Int, Int]]) {

  private var resultWeight = -1
  private var resultPrice = -1
  private var result: Seq[Int] = Seq.empty
  private var steps: Long = 0L

  def bruteForce = {
    solveKnapsackRecursively(0, Array.fill(count)(0))
    (resultWeight, resultPrice, result, steps)
  }

  def priceWeightHeuristic = {
    solveKnapsackWithPriceWeightHeuristic
    (resultWeight, resultPrice, result, steps)
  }

  def branchAndBound = {
    solveKnapsackRecursivelyWithBB(0, Array.fill(count)(0))
    (resultWeight, resultPrice, result, steps)
  }

  def dynamicProgramming = {
    val table = Array.tabulate[Int](maxWeight + 1, items.size + 1)((x, y) => if (x == 0 || y == 0) 0 else -1)

    for (i <- 1 to items.size) {
      for (j <- 1 to maxWeight) {
        if (items(i - 1)._1 <= j) {
          val bi = items(i - 1)._2
          val wi = items(i - 1)._1
          if ((bi + table(j - wi)(i - 1)) > table(j)(i - 1)) {
            table(j)(i) = bi + table(j - wi)(i - 1)
          }
          else {
            table(j)(i) = table(j)(i - 1)
          }
        }
        else {
          table(j)(i) = table(j)(i - 1)
        }
      }
    }

    var res = ""
    var i = items.size
    var k = maxWeight
    while (i > 0 && k > 0) {
      if (table(k)(i) != table(k)(i - 1)) {
        k -= items(i - 1)._1
        i -= 1
        res += "1"
      }
      else {
        i -= 1
        res += "0"
      }
    }

    if (res.length != items.size) {
      res = res.padTo(items.size, '0')
    }

    val finalResult = res.reverse.toList.map(x => if (x == '0') 0 else 1)
    val computed = computeItem(finalResult)
    (computed._1, computed._2, finalResult, maxWeight * items.size)
  }

  def aprox(param: Double) = {
    val items = this.items.transform(x => ((x._1 / param).ceil.toInt, x._2))
    val maxWeight = (this.maxWeight / param).ceil.toInt

    val table = Array.tabulate[Int](maxWeight + 1, items.size + 1)((x, y) => if (x == 0 || y == 0) 0 else -1)

    for (i <- 1 to items.size) {
      for (j <- 1 to maxWeight) {
        if (items(i - 1)._1 <= j) {
          val bi = items(i - 1)._2
          val wi = items(i - 1)._1
          if ((bi + table(j - wi)(i - 1)) > table(j)(i - 1)) {
            table(j)(i) = bi + table(j - wi)(i - 1)
          }
          else {
            table(j)(i) = table(j)(i - 1)
          }
        }
        else {
          table(j)(i) = table(j)(i - 1)
        }
      }
    }

    var res = ""
    var i = items.size
    var k = maxWeight
    while (i > 0 && k > 0) {
      if (table(k)(i) != table(k)(i - 1)) {
        k -= items(i - 1)._1
        i -= 1
        res += "1"
      }
      else {
        i -= 1
        res += "0"
      }
    }

    if (res.length != items.size) {
      res = res.padTo(items.size, '0')
    }

    val finalResult = res.reverse.toList.map(x => if (x == '0') 0 else 1)
    val computed = computeItem(finalResult)
    (computed._1, computed._2, finalResult)
  }

  private def solveKnapsackWithPriceWeightHeuristic(): Unit = {
    val indexedItems = items.zipWithIndex
    val sortedItems = indexedItems.sortWith((x, y) => if (x._1._2.toDouble / x._1._1 > y._1._2.toDouble / y._1._1) true else false)
    var weight = 0
    var price = 0
    var res = Array.fill(count)(0)
    for ((item, index) <- sortedItems) {
      if (weight + item._1 <= maxWeight) {
        weight += item._1;
        price += item._2;
        res = res.updated(index, 1)
        steps += 1
      }
    }

    resultWeight = weight
    resultPrice = price
    result = res
  }

  private def solveKnapsackRecursively(index: Int, res: Seq[Int]): Unit = {
    val resPair = computeItem(res)

    steps += 1

    if (resPair._1 <= maxWeight && resPair._2 > resultPrice) {
      resultWeight = resPair._1
      resultPrice = resPair._2
      result = res
    }

    if (resPair._1 > maxWeight) {
      return
    }

    if (index + 1 <= count) {
      solveKnapsackRecursively(index + 1, res.updated(index, 0))

      solveKnapsackRecursively(index + 1, res.updated(index, 1))
    }
  }

  private def solveKnapsackRecursivelyWithBB(index: Int, res: Seq[Int]): Unit = {
    val resPair = computeItem(res)

    steps += 1

    if (resPair._1 <= maxWeight && resPair._2 > resultPrice) {
      resultWeight = resPair._1
      resultPrice = resPair._2
      result = res
    }

    if (resPair._1 > maxWeight) {
      return
    }

    var bbRes = res
    var i = index + 1
    while (i < count) {
      bbRes = bbRes.updated(i, 1)
      i += 1
    }
    if (computeItem(bbRes)._2 <= resultPrice) {
      return
    }

    if (index + 1 <= count) {
      solveKnapsackRecursivelyWithBB(index + 1, res.updated(index, 0))

      solveKnapsackRecursivelyWithBB(index + 1, res.updated(index, 1))
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