package solution

import scala.collection.mutable.ListBuffer

object Solution extends Solution {

  def main(args: Array[String]): Unit = {
    val result = println(solution("1452 34 545 - DUP"))
  }
}

class Solution {

  //Stack where operations generates results
  var stack = ListBuffer[Int]()

  def solution(s: String): Int = {
    // Splitting string into List to have a list of operations
    val operations = s.split(" ").toList
    try {
      operations.foreach{calculateResult(_)}
    } catch {
      case e => return -1
    }
    return getLastElement
  }

  /**
    * Method for operation result calculation
    *
    * @param value
    * @return
    */
  def calculateResult(value: String): ListBuffer[Int] = {
    value match {
      case "POP" => remove()
      case "DUP" => duplicate()
      case "+" => sum()
      case "-" => subs()
      case _ => addToStack(value.toInt)
    }
  }

  /**
    * Adds value to stack
    * @param value
    */
  def addToStack(value: Int): ListBuffer[Int] = {
    if (value > Math.pow(2, 20) - 1) {
      throw new IllegalStateException("Overflow")
    }
    stack += value.toInt
  }

  /**
    * Remove last element
    * @return
    */
  def remove() = {
    if (stack.size < 1) {
      throw new IllegalStateException("Stack empty exception")
    }
    stack = stack.dropRight(1)
    stack
  }

  /**
    * Duplicate last element
    * @return
    */
  def duplicate() = {
    val lastest = getLastElement
    stack += lastest
  }

  /**
    * Sums lasts two elements
    * @return
    */
  def sum() = {
    val lastest = getLastElement
    remove()
    val laster = getLastElement
    remove()
    stack += lastest + laster
  }

  /**
    * Subtract lasts two elements
    * @return
    */
  def subs() = {
    val lastest = getLastElement
    remove()
    val laster = getLastElement
    remove()
    stack += lastest - laster
  }

  /**
    * Gets last element of List or throw exception if empty
    * @return
    */
  private def getLastElement = {
    if (stack.size < 1) {
      throw new IllegalStateException("Stack empty exception")
    }
    stack.last
  }
}
