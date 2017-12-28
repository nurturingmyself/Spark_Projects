package com.explore.interview.questions

import scala.collection.immutable.Stream.Empty
import scala.util.control.Breaks._
object ParanthesesBalancing {


  def main(args: Array[String]) {
    /*println("balance: '(if (zero? x) max (/ 1 x))' is balanced : "+balanceParanthesisNonRecursively("(if (zero? x) max (/ 1 x))".toList))
    println("balance: 'I told him ...' is balanced : "+balanceParanthesisNonRecursively("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    println("balance: ':-)(' is unbalanced : "+balanceParanthesisNonRecursively(":-)".toList))
    println("balance: counting is not enough : "+balanceParanthesisNonRecursively(")(".toList))
*/
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("pascal(0,1)-->" + pascal(0, 1))
    println("pascal(0,1)-->" + pascal(0, 1))
    println("pascal(0,1)-->" + pascal(0, 1))
  }
  /**
    *
    */
  def balance(charList: List[Char]): Boolean = {
    //check if parantheses are balanced
    def isParanthesesbalanced(chars: List[Char], numberOfOpens: Int): Boolean = {
      if (chars.isEmpty)
        numberOfOpens == 0
      else if (chars.head == '(')
        isParanthesesbalanced(chars.tail,numberOfOpens+1)
      else if (chars.head == ')')
        numberOfOpens > 0 && isParanthesesbalanced(chars.tail,numberOfOpens-1)
      else
        isParanthesesbalanced(chars.tail,numberOfOpens)
    }
    //call function
    isParanthesesbalanced(charList,0)
  }

  case class TestRam(var r1:String)

  val tr = new TestRam("Hi")
  tr.r1 = "test"

  def balanceParanthesis(strVal: List[Char]) : Boolean = {
      def isStringBalanced(strVal: List[Char], numberOfOpenParam:Int): Boolean = {
        if(strVal.isEmpty)
          numberOfOpenParam == 0
        else if(strVal.head == '(')
          isStringBalanced(strVal.tail, numberOfOpenParam+1)
        else if(numberOfOpenParam > 0 && strVal.head == ')')
          isStringBalanced(strVal.tail, numberOfOpenParam-1)
        else
          isStringBalanced(strVal.tail, numberOfOpenParam)
    }
    true
  }

  def balanceParanthesisNonRecursively(strVal:List[Char]):Boolean = {
    var numberOfOpens = 0
    for(character <- strVal){
      breakable {
        if(numberOfOpens < 0)
          break()
        if(character.equals('('))
          numberOfOpens += 1
        else if(character.equals(')'))
          numberOfOpens -= 1
      }
    }
    numberOfOpens == 0
  }

  def pascal(x: Int, y: Int): Int = {
    if (x < 0 || y < 0 || x > y)
      throw new IllegalArgumentException("Column and row numbers must be 0 or greater. Column length must be lower than row length")
    else {
      if (x == y || x == 0)
        1
      else
        pascal(x - 1, y - 1) + pascal(x, y - 1)
    }
  }

}