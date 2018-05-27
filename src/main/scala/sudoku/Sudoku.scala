package sudoku

/** The main class */
object Sudoku extends Sudoku {

  /** Main function */
  def main(args: Array[String]): Unit = {

    val grid = Array(Array('.','.','.','1','4','.','.','2','.'),
      Array('.','.','6','.','.','.','.','.','.'),
      Array('.','.','.','.','.','.','.','.','.'),
      Array('.','.','1','.','.','.','.','.','.'),
      Array('.','6','7','.','.','.','.','.','9'),
      Array('.','.','.','.','.','.','8','1','.'),
      Array('.','3','.','.','.','.','.','.','6'),
      Array('.','.','.','.','.','7','.','.','.'),
      Array('.','.','.','5','.','.','.','7','.'))

    println(sudoku2(grid))
  }
}

class Sudoku extends Serializable {
  def sudoku2(grid: Array[Array[Char]]): Boolean = {

    val subMarix = 3

    def rotateImage(a: Array[Array[Char]]): Array[Array[Char]] = {
      a.transpose.map(line => line.reverse)
    }

    def firstDuplicate(a: Array[Char]): Int = {
      def getDup(l: List[Char], s: Set[Char]): Int = l match {
        case Nil => -1
        case x :: xs => if (s.contains(x) && x != '.') x
        else getDup(xs, s + x)
      }

      getDup(a.toList, Set())
    }

    def checkSubGridsDup(grid: Array[Array[Char]], lenght: Int, weight: Int, subMatrix: Int): Boolean = {
      if (lenght - subMatrix < 0) {
        return true
      } else {
        var tempArray: Array[Char] = Array.fill[Char](9)(0)
        var k = 0
        for (i <- lenght - subMatrix to lenght - 1) {
          for (j <- weight - subMatrix to weight - 1) {
            tempArray(k) = grid(i)(j)
            k = k + 1
          }
        }
        if (firstDuplicate(tempArray) == -1) {
          if (weight > subMatrix) {
            checkSubGridsDup(grid, lenght, weight - subMatrix, subMatrix)
          } else {
            checkSubGridsDup(grid, lenght - subMatrix, grid(0).size, subMatrix)
          }
        } else {
          return false
        }
      }
    }

    val checkFilesDup = grid.map(line => firstDuplicate(line))
    val checkColumnsDup = rotateImage(grid).map(line => firstDuplicate(line))

    if (checkFilesDup.forall(_ == -1) &&
      checkColumnsDup.forall(_ == -1) &&
      checkSubGridsDup(grid, grid.size, grid(0).size, subMarix)) {
      return true
    } else {
      return false
    }
  }
}