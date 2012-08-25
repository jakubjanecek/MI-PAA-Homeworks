package cz.cvut.fit.mi_paa.janecjak

import scala.io.Source
import java.io.{PrintWriter, File}

object Knapsack {
  def main(args: Array[String]): Unit = {
    new File("results").mkdir();

    for (param <- Array(2, 4, 8, 16)) {
      val w = new PrintWriter("results/aprox" + param + ".csv")
      for (file <- new File("data").listFiles() if file.isFile if file.getName.endsWith("inst.dat")) {

        //        w.println("Price\tResult\tTime(ns)")
        //      w.println("ID\tn\tPrice\tResult\tTime(ns)")
        for (line <- Source.fromFile(file.getAbsolutePath).getLines) {
          val Array(id, n, m, rest@_*) = line.split(" ")
          val groupedItems = rest.grouped(2)
          val items = groupedItems.toArray.map(x => (x.head.toInt, x.tail.head.toInt))

          //        val start1 = System.nanoTime
          //        val result1 = new Knapsack(n.toInt, m.toInt, items).bruteForce
          //        println(result1._3.mkString(" "))
          //        val end1 = System.nanoTime
          //        val time1 = end1 - start1;
          //        println(time1)

          //        val start2 = System.nanoTime
          //        val result2 = new Knapsack(n.toInt, m.toInt, items).priceWeightHeuristic
          //        println(result2)
          //        val end2 = System.nanoTime
          //        val time2 = end2 - start2;

          //        val start3 = System.nanoTime
          //        val result = new Knapsack(n.toInt, m.toInt, items).branchAndBound
          //        val end3 = System.nanoTime
          //        val time = end3 - start3

          //        val start4 = System.nanoTime
          //        val result = new Knapsack(n.toInt, m.toInt, items).dynamicProgramming
          //        val end4 = System.nanoTime
          //        val time = end4 - start4

          val start5 = System.nanoTime
          val result = new Knapsack(n.toInt, m.toInt, items).aprox(param)
          val end5 = System.nanoTime
          val time = end5 - start5

          //        w.println(id + "\t" + n + "\t" + result._2 + "\t" + result._3.mkString(" ") + "\t" + time)
          w.println(result._2 + "\t" + result._3.mkString(" ") + "\t" + time)
        }
      }
      w.close
    }
  }
}

class Knapsack(private val count: Int, private val maxWeight: Int, private val items: Array[Tuple2[Int, Int]]) {

  private var resultWeight = -1
  private var resultPrice = -1
  private var result: Seq[Int] = Seq.empty

  def bruteForce = {
    solveKnapsackRecursively(0, Array.fill(count)(0))
    (resultWeight, resultPrice, result)
  }

  def priceWeightHeuristic = {
    solveKnapsackWithPriceWeightHeuristic
    (resultWeight, resultPrice, result)
  }

  def branchAndBound = {
    solveKnapsackRecursivelyWithBB(0, Array.fill(count)(0))
    (resultWeight, resultPrice, result)
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
    (computed._1, computed._2, finalResult)
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
      }
    }

    resultWeight = weight
    resultPrice = price
    result = res
  }

  private def solveKnapsackRecursively(index: Int, res: Seq[Int]): Unit = {
    val resPair = computeItem(res)

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
