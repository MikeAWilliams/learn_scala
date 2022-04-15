def markProblem(number: Int) {
   println(s"\n\n------------------- problem $number ---------------------------------\n\n")
}
markProblem(1)
def binarySearch(items: Array[Int], target: Int):Int = {
    def binarySearchRecursive(items: Array[Int], target: Int, startIndex: Int, stopIndex: Int):Int = {
        val midIndex = (stopIndex + startIndex)/2
        //println(s"$depth $startIndex $stopIndex $midIndex")
        if(startIndex >= stopIndex) -1
        else if(items(midIndex) == target) midIndex
        else if(items(midIndex) > target) binarySearchRecursive(items, target, startIndex, midIndex)
        else binarySearchRecursive(items, target, midIndex + 1, stopIndex)
    }
    binarySearchRecursive(items, target, 0, items.length)
}
println(binarySearch(Array(1, 3, 7, 9, 13), 3))
println(binarySearch(Array(1, 3, 7, 9, 13), 9))
println(binarySearch(Array(1, 3, 7, 9, 13), 2))

markProblem(2)

def binarySearchGeneric[T: Ordering](items: IndexedSeq[T], target: T):Int = {
    def binarySearchRecursive[T: Ordering](items: IndexedSeq[T], target: T, startIndex: Int, stopIndex: Int) :Int = {
        val midIndex = (stopIndex + startIndex)/2
        val compareResult = Ordering[T].compare(target, items(midIndex))
        if(startIndex == stopIndex) -1
        else if(compareResult == 0) midIndex
        else if(compareResult < 0) binarySearchRecursive(items, target, startIndex, midIndex)
        else binarySearchRecursive(items, target, midIndex + 1, stopIndex)
    }
    binarySearchRecursive(items, target, 0, items.length)
}
println(binarySearchGeneric(Array(1, 3, 7, 9, 13), 3))
println(binarySearchGeneric(Array(1, 3, 7, 9, 13), 9))
println(binarySearchGeneric(Array(1, 3, 7, 9, 13), 2))