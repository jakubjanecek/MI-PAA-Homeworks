package cz.cvut.fit.mi_paa.janecjak

import java.io.File
import io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Set

object Buckets {
  def main(args: Array[String]): Unit = {
    for (file <- new File("data").listFiles() if file.isFile if file.getName.endsWith("inst.dat")) {
      for (line <- Source.fromFile(file.getAbsolutePath).getLines) {
        if (line.length() > 0) {
          val Array(id, nString, rest@_*) = line.replaceAll("  ", " ") split (" ")
          val n = nString.toInt
          val capacities = rest.take(n).map(_.toInt)
          val startingCapacities = rest.drop(n).take(n).map(_.toInt)
          val finalCapacities = rest.drop(2 * n).take(n).map(_.toInt)
          //          new BFS(n, capacities, startingCapacities, finalCapacities).solve
          new Heuristic(n, capacities, startingCapacities, finalCapacities).solve
        }
      }
    }
  }
}

class BFS(n: Int, capacities: Seq[Int], startingCapacities: Seq[Int], finalCapacities: Seq[Int]) {

  val queue = Queue[Tuple2[Seq[Int], Long]]()
  val generated = Set[Seq[Int]]() // visited
  var trials = 0L

  queue.enqueue((startingCapacities, 0L))

  def solve: Unit = {
    var configuration = queue.dequeueFirst(x => true)
    while (configuration != None) {
      trials += 1

      if (configuration.get._1 == finalCapacities) {
        println(configuration.get._2 + "\t" + trials)
        return
      }

      generate(configuration.get)

      configuration = queue.dequeueFirst(x => true)
    }
    println("NOT found")
  }

  private def generate(tuple: Tuple2[Seq[Int], Long]) {
    val c = tuple._1
    val steps = tuple._2
    for (i <- 0 until n) {
      val c1 = c.updated(i, capacities(i))
      if (!generated.contains(c1)) {
        queue.enqueue((c1, steps + 1))
        generated.add(c1)
      }
    }

    for (i <- 0 until n) {
      val c2 = c.updated(i, 0)
      if (!generated.contains(c2)) {
        queue.enqueue((c2, steps + 1))
        generated.add(c2)
      }
    }

    for (i <- 0 until n) {
      for (j <- 0 until n) {
        var c3: Seq[Int] = null
        if (capacities(j) - c(j) >= c(i)) {
          val temp = c.updated(j, c(j) + c(i))
          c3 = temp.updated(i, 0)
        }
        else {
          val temp = c.updated(j, capacities(j))
          c3 = temp.updated(i, c(i) - (capacities(j) - c(j)))
        }

        if (!generated.contains(c3)) {
          queue.enqueue((c3, steps + 1))
          generated.add(c3)
        }
      }
    }
  }
}

class Heuristic(n: Int, capacities: Seq[Int], startingCapacities: Seq[Int], finalCapacities: Seq[Int]) {

  implicit def heuristic: Ordering[(Seq[Int], Long)] = new Ordering[(Seq[Int], Long)] {
    def compare(c: (Seq[Int], Long), other: (Seq[Int], Long)) = {
      val dist1 = computeDistance(c._1) - computeValue(c._1)
      val dist2 = computeDistance(other._1) - computeValue(other._1)

      -dist1.compare(dist2)
    }

    private def computeDistance(c: Seq[Int]) = {
      import java.lang.Math

      var sum = 0
      for (i <- 0 until n) {
        sum += Math.abs(c(i) - finalCapacities(i))
      }
      sum
    }

    private def computeValue(c: Seq[Int]) = {
      var value = 0
      for (i <- 0 until n) {
        if (finalCapacities(i) == c(i)) {
          value += 2
        }
        if (finalCapacities.contains(c(i))) {
          value += 1
        }
      }
      value
    }
  }

  val queue = new PriorityQueue[(Seq[Int], Long)]()(heuristic)
  val generated = Set[Seq[Int]]()
  var trials = 0L

  queue.enqueue((startingCapacities, 0L))

  def solve: Unit = {
    while (true) {
      var configuration = queue.dequeue()
      trials += 1

      if (configuration._1 == finalCapacities) {
        println(configuration._2 + "\t" + trials)
        return
      }

      generate(configuration)
    }
    println("NOT found")
  }

  private def generate(tuple: (Seq[Int], Long)) {
    val c = tuple._1
    val steps = tuple._2

    for (i <- 0 until n) {
      val c1 = c.updated(i, capacities(i))
      if (!generated.contains(c1)) {
        queue.enqueue((c1, steps + 1))
        generated.add(c1)
      }
    }

    for (i <- 0 until n) {
      val c2 = c.updated(i, 0)
      if (!generated.contains(c2)) {
        queue.enqueue((c2, steps + 1))
        generated.add(c2)
      }
    }

    for (i <- 0 until n) {
      for (j <- 0 until n) {
        var c3: Seq[Int] = null
        if (capacities(j) - c(j) >= c(i)) {
          val temp = c.updated(j, c(j) + c(i))
          c3 = temp.updated(i, 0)
        }
        else {
          val temp = c.updated(j, capacities(j))
          c3 = temp.updated(i, c(i) - (capacities(j) - c(j)))
        }

        if (!generated.contains(c3)) {
          queue.enqueue((c3, steps + 1))
          generated.add(c3)
        }
      }
    }
  }
}
