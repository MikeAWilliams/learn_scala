def printSeperatorMessage(message: String) = {
   println(s"\n\n-------------------------- $message --------------------\n\n")
}
println(1 + 1) // 2 myScript.sc
println("hello" + " " + "world") // hello world
println(List("I", "am", "cow")) // List(I,am,cow)

def hello(n: Int) = { 
"hello scala world" + "!" * n
}

printSeperatorMessage("sodoku")

val grid = Array(
Array(5, 3, 4, 6, 7, 8, 9, 1, 2),
Array(6, 7, 2, 1, 9, 5, 3, 4, 8),
Array(1, 9, 8, 3, 4, 2, 5, 6, 7),
Array(8, 5, 9, 7, 6, 1, 4, 2, 3),
Array(4, 2, 6, 8, 5, 3, 7, 9, 1),
Array(7, 1, 3, 9, 2, 4, 8, 5, 6),
Array(9, 6, 1, 5, 3, 7, 2, 8, 4),
Array(2, 8, 7, 4, 1, 9, 6, 3, 5),
Array(3, 4, 5, 2, 8, 6, 1, 7, 9)
)
for (i <- Range(0,9)) {
   val square = Range(0, 9).map(j => {
      val index1 = (i % 3) * 3 + j % 3
      val index2 = (i / 3) * 3 + j / 3
      println(s"$index1,$index2")
      grid(index1)(index2)
   })
   println(square)
}

printSeperatorMessage("implicit exersize")

trait StrParser[T]{ def parse(s: String): T }
object StrParser{
   implicit object ParseInt extends StrParser[Int]{
      def parse(s: String) = s.toInt
   }
   implicit object ParseBoolean extends StrParser[Boolean]{
      def parse(s: String) = s.toBoolean
   }
   implicit object ParseDouble extends StrParser[Double]{
      def parse(s: String) = s.toDouble
   }
}

def parseFromString[T: StrParser](s: String) = {
   implicitly[StrParser[T]].parse(s)
}
println(parseFromString[Int]("7"))