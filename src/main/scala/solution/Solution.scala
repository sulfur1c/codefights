package solution

import scala.collection.mutable.ListBuffer

object Solution extends Solution {

  def main(args: Array[String]): Unit = {
    val result = println(solution("54 - DUP 48"))
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
    return stack.last
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
      case _ => stack += value.toInt
    }
  }

  /**
    * Remove last element
    * @return
    */
  def remove() = {
    if (stack.size < 1) {
      throw new IllegalStateException("Exception thrown")
    }else {
      stack.dropRight(1)
    }
  }

  /**
    * Duplicate last element
    * @return
    */
  def duplicate() = {
    val lastest = stack.last
    stack += lastest
  }

  /**
    * Sums lasts two elements
    * @return
    */
  def sum() = {
    val lastest = stack.last
    remove()
    val laster = stack.last
    remove()
    stack += lastest + laster
  }

  /**
    * Subtract lasts two elements
    * @return
    */
  def subs() = {
    val lastest = stack.last
    remove()
    val laster = stack.last
    remove()
    stack += lastest - laster
  }
}
