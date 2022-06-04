import scala.concurrent._, java.util.concurrent.Executors
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import $ivy.`org.jsoup:jsoup:1.13.1`, org.jsoup._
import collection.JavaConverters._

implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

def mergeSort(items: Array[Int]): Array[Int] = {
  if (items.length <= 1) items
  else {
    val (left, right) = items.splitAt(items.length / 2)
    val (sortedLeft, sortedRight) = (mergeSort(left), mergeSort(right))
    var (leftIdx, rightIdx) = (0, 0)
    val output = Array.newBuilder[Int]
    while (leftIdx < sortedLeft.length || rightIdx < sortedRight.length) {
      val takeLeft = (leftIdx < sortedLeft.length, rightIdx < sortedRight.length) match {
        case (true, false) => true
        case (false, true) => false
        case (true, true) => sortedLeft(leftIdx) < sortedRight(rightIdx)
      }
      if (takeLeft) {
        output += sortedLeft(leftIdx)
        leftIdx += 1
      } else {
        output += sortedRight(rightIdx)
        rightIdx += 1
      }
    }
    output.result()
  }
}
val size = 10000000
val r = scala.util.Random
println("Generating inpuit")
val (input, nsToGenerateInput) = time {
  (for(i <- 0 to size) yield r.nextInt(size)).toArray
}

val secondsForInput = nsToGenerateInput.toUnit(TimeUnit.SECONDS)
println(s"time to get input $secondsForInput seconds")
val (result, duration) = time {
    mergeSort(input)
}
val seconds = duration.toUnit(TimeUnit.SECONDS)
println(s"time to get futures $seconds seconds")