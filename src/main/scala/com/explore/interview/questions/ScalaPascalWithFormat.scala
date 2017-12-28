package com.explore.interview.questions
import scala.math.max

object Int {
  def unapply(s:String) : Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _ : java.lang.NumberFormatException => None
  }
}

object ScalaPascalWithFormat {


  val usage =
    """
    Usage: Pascal rows
  """

  def main(args: Array[String]) {
    if (args.length == 0)
      println(usage)

    for (a <- args) a match {
      case Int(r) => printTriangle(r)
      case s: String => println(s + " is not a whole number")
    }
  }

  /**
    * Computes the value at row r and column c
    */
  def v(r: Int, c: Int): Int =
    if (c == 0)
      1
    else
      v(r, c - 1) * (r - c) / c

  def printRow(r: Int, seplen: Int) = {
    for (col <- 0 to r) {
      if (col > 0)
        print(" " * seplen)
      val n = v(r + 1, col)
      print((" " * (seplen - n.toString.length)) + n)
    }
    println()
  }

  def printTriangle(rows: Int) = {
    // Figure out how many spaces to separate numbers
    var largest = 1
    for (col <- 0 to rows)
      largest = max(largest, v(rows + 1, col))

    val seplen = largest.toString.length
    val sep = " " * seplen

    for (r <- 0 to rows) {
      print(sep * (rows - r))
      printRow(r, seplen)
    }
  }
}
