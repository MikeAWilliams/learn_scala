class Trie() {
  class Node(var hasValue: Boolean,
             val children: collection.mutable.Map[Char, Node] = collection.mutable.Map())
  val root = new Node(false)
  def add(s: String) = {
    var current = root
    for (c <- s) current = current.children.getOrElseUpdate(c, new Node(false))
    current.hasValue = true
  }
  def contains(s: String): Boolean = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    current.exists(_.hasValue)
  }
  // this method is weird to me. I feel like it is an implementation detail and should be hidden
  def prefixesMatchingString0(s: String): Set[Int] = {
    var current = Option(root)
    val output = Set.newBuilder[Int]
    for ((c, i) <- s.zipWithIndex if current.nonEmpty) {
      var childrenOfCurrent = ""
      for ((k,v)<-current.get.children) childrenOfCurrent += k
      println(s"$i $childrenOfCurrent")
      if (current.get.hasValue) output += i
      current = current.get.children.get(c)
    }
    if (current.exists(_.hasValue)) output += s.length
    output.result()
  }
  // this returns a set because prefixMatchingString0 returns a set and maping over a set yields a set
  def prefixesMatchingString(s: String): Set[String] = {
    prefixesMatchingString0(s).map(s.substring(0, _))
  }
  def stringsMatchingPrefix(s: String): Set[String] = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c) // initial walk
    if (current.isEmpty) Set()
    else {
      val output = Set.newBuilder[String]
      // notice that List is an imutable linked list
      // lists share tails so as we prepend to the front, all the words share most of the list
      def recurse(current: Node, path: List[Char]): Unit = {
        if (current.hasValue) output += (s + path.reverse.mkString)
        // :: returns a new list with the new element at the front and shareing all the other elementes.
        // need to reverse because we are prepending all the time
        for ((c, n) <- current.children) recurse(n, c :: path) 
      }
      recurse(current.get, Nil) // recursive walk
      output.result()
    }
  }
}

val t = new Trie(); t.add("mango"); t.add("mandarin"); t.add("map"); t.add("man"); t.add("m"); t.add("nine")
val res1 = t.prefixesMatchingString0("mangosteen")
println(res1)