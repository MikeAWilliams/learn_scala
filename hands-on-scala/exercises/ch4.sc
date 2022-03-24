def printSeperatorMessage(message: String) = {
   println(s"\n\n-------------------------- $message --------------------\n\n")
}

printSeperatorMessage("Zero sudoku")

def isValidSudoku(grid: Array[Array[Int]]): Boolean = {
   !Range(0, 9).exists {i =>
      val row = Range(0, 9).map(grid(i)(_)).filter(_ > 0)
      val col = Range(0, 9).map(grid(_)(i)).filter(_ > 0)
      val square = Range(0, 9).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3)).filter(_ > 0)
      row.distinct.length != row.length ||
      col.distinct.length != col.length ||
      square.distinct.length != square.length
   }
}

val oneIsValid = isValidSudoku(Array(
   Array(5, 3, 4, 6, 7, 8, 9, 1, 2),
   Array(6, 7, 2, 1, 9, 5, 3, 4, 8),
   Array(1, 9, 8, 3, 4, 2, 5, 6, 7),
   Array(8, 5, 9, 7, 6, 1, 4, 2, 3),
   Array(4, 2, 6, 8, 5, 3, 7, 9, 1),
   Array(7, 1, 3, 9, 2, 4, 8, 5, 6),
   Array(9, 6, 1, 5, 3, 7, 2, 8, 4),
   Array(2, 8, 7, 4, 1, 9, 6, 3, 5),
   Array(3, 4, 5, 2, 8, 6, 1, 7, 9)
))

println(oneIsValid)

 val twoIsValid = isValidSudoku(Array(
   Array(5, 3, 4, 6, 7, 8, 9, 1, 2),
   Array(6, 7, 2, 1, 9, 5, 3, 4, 8),
   Array(1, 9, 8, 3, 4, 2, 5, 6, 7),
   Array(8, 5, 9, 7, 6, 1, 4, 2, 3),
   Array(4, 2, 6, 8, 5, 3, 7, 9, 1),
   Array(7, 1, 3, 9, 2, 4, 8, 5, 6),
   Array(9, 6, 1, 5, 3, 7, 2, 8, 4),
   Array(2, 8, 7, 4, 1, 9, 6, 3, 5),
   Array(3, 4, 5, 2, 8, 6, 1, 7, 8)
)) // bottom right cell should be 9

println(twoIsValid)

val threeIsValid = isValidSudoku(Array(
   Array(3, 1, 6, 5, 7, 8, 4, 9, 2),
   Array(5, 2, 9, 1, 3, 4, 7, 6, 8),
   Array(4, 8, 7, 6, 2, 9, 5, 3, 1),
   Array(2, 6, 3, 0, 1, 0, 0, 8, 0),
   Array(9, 7, 4, 8, 6, 3, 0, 0, 5),
   Array(8, 5, 1, 0, 9, 0, 6, 0, 0),
   Array(1, 3, 0, 0, 0, 0, 2, 5, 0),
   Array(0, 0, 0, 0, 0, 0, 0, 7, 4),
   Array(0, 0, 5, 2, 0, 6, 3, 0, 0)
))

println(threeIsValid)

val fourIsValid = isValidSudoku(Array(
   Array(3, 1, 6, 5, 7, 8, 4, 9, 3),
   Array(5, 2, 9, 1, 3, 4, 7, 6, 8),
   Array(4, 8, 7, 6, 2, 9, 5, 3, 1),
   Array(2, 6, 3, 0, 1, 0, 0, 8, 0),
   Array(9, 7, 4, 8, 6, 3, 0, 0, 5),
   Array(8, 5, 1, 0, 9, 0, 6, 0, 0),
   Array(1, 3, 0, 0, 0, 0, 2, 5, 0),
   Array(0, 0, 0, 0, 0, 0, 0, 7, 4),
   Array(0, 0, 5, 2, 0, 6, 3, 0, 0)
)) // top right cell should be 2

println(fourIsValid)

printSeperatorMessage("render sudoku nonidomatic")

def renderSudokuNonIdeomatic(grid: Array[Array[Int]]) = {
   println("+-------+-------+-------+")
   for (rowIndex <- Range(0,9)) {
    val row = Range(0, 9).map(grid(rowIndex)(_))
    var line = "| "
    for (column <- Range(0,9)){
      var value = row(column).toString()
      if ("0" == value) value = " "
      line += value + " "
      if ((column + 1) % 3 == 0) line += "| "
    }
    println(line)
    if ((rowIndex + 1) % 3 == 0) println("+-------+-------+-------+")
   }
}
renderSudokuNonIdeomatic(Array(
   Array(3, 1, 6, 5, 7, 8, 4, 9, 2),
   Array(5, 2, 9, 1, 3, 4, 7, 6, 8),
   Array(4, 8, 7, 6, 2, 9, 5, 3, 1),
   Array(2, 6, 3, 0, 1, 0, 0, 8, 0),
   Array(9, 7, 4, 8, 6, 3, 0, 0, 5),
   Array(8, 5, 1, 0, 9, 0, 6, 0, 0),
   Array(1, 3, 0, 0, 0, 0, 2, 5, 0),
   Array(0, 0, 0, 0, 0, 0, 0, 7, 4),
   Array(0, 0, 5, 2, 0, 6, 3, 0, 0)
))

printSeperatorMessage("render sudoku nonidomatic")

def renderSudoku(grid: Array[Array[Int]]) = {
   println("+-------+-------+-------+")
   for (rowIndex <- Range(0,9)) {
    val row = Range(0, 9).map(grid(rowIndex)(_))
    var line = "| "
    for (column <- Range(0,9)){
      var value = row(column).toString()
      if ("0" == value) value = " "
      line += value + " "
      if ((column + 1) % 3 == 0) line += "| "
    }
    println(line)
    if ((rowIndex + 1) % 3 == 0) println("+-------+-------+-------+")
   }
}
renderSudoku(Array(
   Array(3, 1, 6, 5, 7, 8, 4, 9, 2),
   Array(5, 2, 9, 1, 3, 4, 7, 6, 8),
   Array(4, 8, 7, 6, 2, 9, 5, 3, 1),
   Array(2, 6, 3, 0, 1, 0, 0, 8, 0),
   Array(9, 7, 4, 8, 6, 3, 0, 0, 5),
   Array(8, 5, 1, 0, 9, 0, 6, 0, 0),
   Array(1, 3, 0, 0, 0, 0, 2, 5, 0),
   Array(0, 0, 0, 0, 0, 0, 0, 7, 4),
   Array(0, 0, 5, 2, 0, 6, 3, 0, 0)
))