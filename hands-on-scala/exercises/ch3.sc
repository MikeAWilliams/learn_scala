def markProblem(number: Int) {
   println(s"\n\n------------------- problem $number ---------------------------------\n\n")
}
markProblem(1)

def flexibleFizzBuzz(min: Int, max: Int)(callback: String => Unit) = {
   val fizzbuzz = for (i <- Range.inclusive(min, max)) {
      callback(
      if (i % 3 == 0 && i % 5 == 0) "FizzBuzz"
      else if (i % 3 == 0) "Fizz"
      else if (i % 5 == 0) "Buzz"
      else i.toString
      )
   }
}
flexibleFizzBuzz(1, 3)(s => println(s))
var i = 0
val output = new Array[String](100)
flexibleFizzBuzz(1,100)(s => {
   output(i) = s
   i += 1
})
for (i <- Range.inclusive(0,99)) {
   println(output(i))
}

markProblem(2)

class Msg(val id: Int, val parent: Option[Int], val txt: String)

def printMessages(messages: Array[Msg]): Unit = {
   def recursiveHelper(parent: Option[Int], indent: String): Unit = {
      for(message <- messages if parent == message.parent){
         println(indent+message.txt)
         recursiveHelper(Some(message.id),indent + "  ")
      }
   }
   recursiveHelper(None, "")
}

printMessages(Array(
  new Msg(0, None, "Hello"),
  new Msg(1, Some(0), "World"),
  new Msg(2, None, "I am Cow"),
  new Msg(3, Some(2), "Hear me moo"),
  new Msg(4, Some(2), "Here I stand"),
  new Msg(5, Some(2), "I am Cow"),
  new Msg(6, Some(5), "Here me moo, moo"),
  new Msg(7, Some(1), "Hey I am a late child of 1"),
  new Msg(8, Some(0), "No kidding I am a child of 0")
))

markProblem(3)

def withFileWriter(fileName: String)(doWork: java.io.BufferedWriter => Unit) = {
   val writer = java.nio.file.Files.newBufferedWriter(java.nio.file.Paths.get(fileName))
   doWork(writer)
   writer.close()
}

def withFileReader(fileName: String)(doWork: java.io.BufferedReader => String):String = {
  val reader = java.nio.file.Files.newBufferedReader(java.nio.file.Paths.get(fileName))
  val result = doWork(reader)
  reader.close()
  result
}

withFileWriter("File.txt") { writer => 
   writer.write("Hello\n"); writer.write("World!")
}

val result = withFileReader("File.txt") { reader =>
   reader.readLine() + "\n" + reader.readLine()
}
assert(result == "Hello\nWorld!")