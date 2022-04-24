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

markProblem(3)

def nodeConstructSnipit(inWordIndex: Int, inputs:Seq[String]) {
    val isWord = inputs.exists(_.length == inWordIndex)
    val filteredInputs = inputs.filter(_.length > inWordIndex)
    println(s"$inWordIndex $isWord, $filteredInputs")
    for((childChar, childInputs) <- filteredInputs.groupBy(_.charAt(inWordIndex))) {
        println(s"  $childChar $childInputs")
    }
}

val input = Seq("mango", "mandarin", "map", "man")
nodeConstructSnipit(0, input)
nodeConstructSnipit(1, input)
nodeConstructSnipit(2, input)
nodeConstructSnipit(3, input)
nodeConstructSnipit(4, input)

markProblem(4)

class ImmutableTrie(inputs: Seq[String]) {
  // inWordIndex is the index into a given word. The character there is the character on the given node
  class Node(val inWordIndex: Int, val inputs: Seq[String]) {
    //isWord is a sneeky trick. If any of the words are the same lenght as the inWordIndex then we know this node is the end of a word
    val isWord = inputs.exists(_.length == inWordIndex)
    // children gets the value from the yeild in the for. it is a map of character to node. the char is the value for the node.
    val children = {
      val filteredInputs = inputs.filter(_.length > inWordIndex)
      println(s"$inWordIndex $inputs $filteredInputs")
      for((childChar, childInputs) <- filteredInputs.groupBy(_.charAt(inWordIndex))) 
        yield (childChar, new Node(inWordIndex + 1, childInputs))
    }
  }

  val root = new Node(0, inputs)

  def print(): Unit = {
      def recursivePrint(current: Node, indent: String): Unit = {
          for((char, node) <- current.children) {
              println(s"$indent$char")
              recursivePrint(node, indent+"|")
          }
      }
      recursivePrint(root,"")
  }

  def contains(searchString: String): Boolean = {
    var current = Option(root)
    for (c <- searchString if current.nonEmpty) current = current.get.children.get(c)
    //exits will return false if the option is empty. If not empty it will apply the predicate, in this case meaning the end of a word
    current.exists(_.isWord)
  }

  def prefixesMatchingString0(searchString: String): Set[Int] = {
    var current = Option(root)
    val output = Set.newBuilder[Int]
    for ((c, i) <- searchString.zipWithIndex if current.nonEmpty) {
      if (current.get.isWord) output += i
      current = current.get.children.get(c)
    }
    if (current.exists(_.isWord)) output += searchString.length
    output.result()
  }

  def prefixesMatchingString(searchString: String): Set[String] = {
    prefixesMatchingString0(searchString).map(searchString.substring(0, _))
  }

  def stringsMatchingPrefix(searchString: String): Set[String] = {
    var current = Option(root)
    for (c <- searchString if current.nonEmpty) current = current.get.children.get(c) // initial walk
    if (current.isEmpty) Set()
    else {
      val output = Set.newBuilder[String]
      def recurse(current: Node, path: List[Char]): Unit = {
        if (current.isWord) output += (searchString + path.reverse.mkString)
        for ((c, n) <- current.children) recurse(n, c :: path)
      }
      recurse(current.get, Nil) // recursive walk
      output.result()
    }
  }
}

val t = new ImmutableTrie(input)
//t.print()
println(t.contains("mango"))
println(t.contains("mang"))
println(t.contains("man"))
println(t.contains("mandarin"))
println(t.contains("mandarine"))
val mangoSteenValue = t.prefixesMatchingString("mangosteen")
println(s"mangosteen $mangoSteenValue")
println(t.stringsMatchingPrefix("man"))
println(t.stringsMatchingPrefix("ma"))
println(t.stringsMatchingPrefix("map"))
println(t.stringsMatchingPrefix("mand"))
println(t.stringsMatchingPrefix("mando"))

markProblem(5)

class ImmutableTrieFromScratch(inputs: Seq[String]) {
  // inWordIndex is the index into a given word. The character there is the character on the given node
  class Node(val inWordIndex: Int, val inputs: Seq[String]) {
    val isWord = inputs.exists(inWordIndex == _.length)
    val children = {
      val validChildren = inputs.filter(_.length > inWordIndex)
      for ((rootChar, childInputs) <- validChildren.groupBy(_.charAt(inWordIndex)))
        yield (rootChar, new Node(inWordIndex + 1, childInputs))
    }
  }

  val root = new Node(0, inputs)

  def print(): Unit = {
    def recursivePrinter(current: Node, indent: String): Unit = {
      for((charAtNode, nodeAtChar) <- current.children){
        println(indent + charAtNode)
        recursivePrinter(nodeAtChar, indent+"|")
      }
    }
    recursivePrinter(root, "")
  }

  def contains(searchString: String): Boolean = {
    var current = Option(root)
    for (letter <- searchString if current.nonEmpty) current = current.get.children.get(letter)
    current.exists(aNode => aNode.isWord)
  }

  def indiciesInSearchThatAreWords(searchString: String): Set[Int] = {
    var current = Option(root)
    val output = Set.newBuilder[Int]
    for((searchChar, index) <- searchString.zipWithIndex if current.nonEmpty) {
      if (current.get.isWord) output += index
      current = current.get.children.get(searchChar)
    }
    output.result()
  }

  def wordsMatchingPrefixOfSearchString(searchString: String): Set[String] = {
    indiciesInSearchThatAreWords(searchString).map(searchString.substring(0,_))
  }

  def wordsWithPrefix(searchString: String): Set[String] = {
    var current = Option(root)
    for(searchChar <- searchString if current.nonEmpty) current = current.get.children.get(searchChar)
      if (current.isEmpty) Set()
      else {
        val output = Set.newBuilder[String]
        def recurse(current: Node, path: List[Char]): Unit = {
          if(current.isWord) output += (searchString + path.reverse.mkString)
          else {
            for ((charAtNode, nodeAtChar) <- current.children){
              recurse(nodeAtChar, charAtNode :: path)
            }
          }
        }
        recurse(current.get, Nil) // recursive walk
        output.result()
      }
  } 
}

val t2 = new ImmutableTrieFromScratch(input)
t2.print()
println(t2.contains("mango"))
println(t2.contains("mang"))
println(t2.contains("man"))
println(t2.contains("mandarin"))
println(t2.contains("mandarine"))
val mangoSteenValue2 = t2.wordsMatchingPrefixOfSearchString("mangosteen")
println(s"mangosteen $mangoSteenValue2")
println(t2.wordsWithPrefix("man"))
println(t2.wordsWithPrefix("ma"))
println(t2.wordsWithPrefix("map"))
println(t2.wordsWithPrefix("mand"))
println(t2.wordsWithPrefix("mando"))

markProblem(6)

def searchPathsBFS[T](start: T, graph: Map[T, Seq[T]]): Map[T, List[T]] = { 
  val seen = collection.mutable.Map(start -> List(start))
  val queue = collection.mutable.ArrayDeque(start -> List(start))
  while (queue.nonEmpty) {
    val (current, path) = queue.removeHead()
    println(s"BFS visiting $current")
    for (next <- graph(current) if !seen.contains(next)) {
      val newPath = next :: path
      seen(next) = newPath
      queue.append((next, newPath))
    }
  }
  seen.toMap
}

def searchPathsDFS[T](start: T, graph: Map[T, Seq[T]]): Map[T, List[T]] = { 
  val seen = collection.mutable.Map(start -> List(start))
  val lengths = collection.mutable.Map(start -> 0)
  val queue = collection.mutable.ArrayDeque((start, List(start), 0))
  while (queue.nonEmpty) {
    val (current, path, currentLength) = queue.removeHead()
    println(s"DFS visiting $current")
    for (next <- graph(current) 
      if !seen.contains(next) || !lengths.get(next).exists(_ <= currentLength + 1))
    {
      val newPath = next :: path
      seen(next) = newPath
      queue.prepend((next, newPath, currentLength + 1))
      // using append results in BFS because it puts the new node, which is a child of the current node on the end
      // we need it on the beginning so that it will be next
      //queue.append((next, newPath, currentLength + 1))
    }
  }
  seen.toMap
}

def shortestPath[T](start: T, dest: T, graph: Map[T, Seq[T]]): Seq[T] = {
  //val shortestReversedPaths = searchPathsBFS(start, graph)
  val shortestReversedPaths = searchPathsDFS(start, graph)
  shortestReversedPaths(dest).reverse
}

val exampleGraph = Map(
    "a" -> Seq("d", "b"),
    "b" -> Seq("c"),
    "c" -> Seq("e"),
    "d" -> Seq("e"),
    "e" -> Seq())

println(shortestPath( start = "a", dest = "e", graph = exampleGraph))
println(shortestPath( start = "a", dest = "c", graph = exampleGraph))