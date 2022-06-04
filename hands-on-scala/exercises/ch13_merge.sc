// something in here leaks. After running it several times it starts to just run out of heap space


import scala.concurrent._, java.util.concurrent.Executors
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import $ivy.`org.jsoup:jsoup:1.13.1`, org.jsoup._
import collection.JavaConverters._

implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

def mergeSortParallel[T: Ordering](items: IndexedSeq[T]): IndexedSeq[T] = {
  Await.result(mergeSortParallelRecursive(items), Duration.Inf)
}

def mergeSortParallelRecursive[T: Ordering](items: IndexedSeq[T]): Future[IndexedSeq[T]] = {
  // author puts in an optmization here for small sets
  // result saves about half the parallel time 2.35 vs 1.4 seconds for me
  // it appears pretty well tuned at 16. I tried 20 and it was slower
  if (items.length <= 16) Future.successful(mergeSortSequential(items))
  //if (items.length <= 1) Future.successful(items)
  else {
    val (left, right) = items.splitAt(items.length / 2)
    val zipped = mergeSortParallelRecursive(left).zip(mergeSortParallelRecursive(right))
    // zipped should be a Future[(IndexedSeq,IndexedSeq)]
    // I don't think it evaluates the futures for left and right to produce zipped
    //println(zipped.getClass)
    // map on a future applies the function to the successfull results of the input
    // in this case a tupple of indexedSeq
    // then return the result as a successful future
    // So map must await the future.
    zipped.map{
      case (sortedLeft, sortedRight) => {
        //println(sortedLeft.getClass)
        // sortedLeft and sortedRight are IndexedSeq
        merge(sortedLeft, sortedRight)
      }
    }
  }
}

// I just coppied the authors sequential and used it as a basis for parallel
def mergeSortSequential[T: Ordering](items: IndexedSeq[T]): IndexedSeq[T] = {
  if (items.length <= 1) items
  else {
    val (left, right) = items.splitAt(items.length / 2)
    merge(mergeSortSequential(left), mergeSortSequential(right))
  }
}

def merge[T: Ordering](sortedLeft: IndexedSeq[T], sortedRight: IndexedSeq[T]) = {
  var leftIdx = 0
  var rightIdx = 0
  val output = IndexedSeq.newBuilder[T]
  while (leftIdx < sortedLeft.length || rightIdx < sortedRight.length) {
    val takeLeft = (leftIdx < sortedLeft.length, rightIdx < sortedRight.length) match {
      case (true, false) => true
      case (false, true) => false
      case (true, true) => Ordering[T].lt(sortedLeft(leftIdx), sortedRight(rightIdx))
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

val size = 1000000
val r = scala.util.Random
val input = (for(i <- 0 to size) yield r.nextInt(size)).toArray

val (sequentialResult, sequentialDuration) = time {
    mergeSortSequential(input)
}
val sequentialSeconds = sequentialDuration.toUnit(TimeUnit.SECONDS)
println(s"time sequential $sequentialSeconds seconds")

val (parallelResult, parallelDuration) = time {
    mergeSortParallel(input)
}
val parallelSeconds = parallelDuration.toUnit(TimeUnit.SECONDS)
println(s"time parallel $parallelSeconds seconds")