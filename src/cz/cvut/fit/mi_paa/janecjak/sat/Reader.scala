package cz.cvut.fit.mi_paa.janecjak.sat

import java.io.File
import io.Source
import collection.mutable.ListBuffer

object Reader {

  private val commentRegex = """c .*""".r
  private val skipRegex = """^[%|0]$""".r
  private val initRegex = """p cnf (\d+) (\d+)""".r
  private val weightsRegex = """w ([ \d]+)""".r
  private val clauseRegex = """([ \d-]+)""".r

  def read(file: File) = {
    var numOfVars = -1
    var numOfClauses = -1
    var weights: Array[Int] = null
    val clauses: ListBuffer[Clause] = new ListBuffer[Clause];

    for (line <- Source.fromFile(file).getLines) {
      line match {
        case commentRegex() | skipRegex() => Unit
        case initRegex(vars, clauses) => {
          numOfVars = vars.toInt
          numOfClauses = clauses.toInt
        }
        case weightsRegex(w) => {
          weights = w.split(" ").map(_.toInt)
        }
        case clauseRegex(vars) => {
          val indices = vars.split(" ").map(_.toInt).dropRight(1)
          val variables = new Array[Symbol](numOfVars)
          for (i <- 0 until numOfVars) {
            variables(i) = 'notPresent
          }
          for (index <- indices) {
            if (index >= 0) {
              variables(index - 1) = 'present
            }
            else {
              variables(index.abs - 1) = 'presentNegated
            }
          }
          clauses += new Clause(variables)
        }
      }
    }

    new Formula(clauses.toList, weights)
  }

}
