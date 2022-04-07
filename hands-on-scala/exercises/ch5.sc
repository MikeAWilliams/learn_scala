def markProblem(number: Int) {
   println(s"\n\n------------------- problem $number ---------------------------------\n\n")
}
markProblem(1)

sealed trait Expr
case class BinOp(left: Expr, op: String, right: Expr) extends Expr
case class Literal(value: Int) extends Expr
case class Variable(name: String) extends Expr

def stringify(expr: Expr): String = expr match {
    case BinOp(left, "+", right) => "(" + stringify(left) + "+" + stringify(right) + ")"
    case BinOp(left, "-", right) => "(" + stringify(left) + "-" + stringify(right) + ")"
    case BinOp(left, "*", right) => "(" + stringify(left) + "*" + stringify(right) + ")"
    case Literal(value) => value.toString
    case Variable(name) => name
}

def simplifyMike(expr: Expr): Expr = {
    val res = expr match {
        case BinOp(Literal(left), "+", Literal(right)) => Literal(left + right)
        case BinOp(Literal(left), "-", Literal(right)) => Literal(left - right)
        case BinOp(Literal(left), "*", Literal(right)) => Literal(left * right)

        // the order of these is important. If I put these here instead of above it doesn't simplify the *0 or the +0 cases
        //case BinOp(left, "*", right) => BinOp(simplifyMike(left), "*", simplifyMike(right))
        //case BinOp(left, "-", right) => BinOp(simplifyMike(left), "-", simplifyMike(right))
        //case BinOp(left, "+", right) => BinOp(simplifyMike(left), "+", simplifyMike(right))

        case BinOp(left, "*", Literal(1)) => simplifyMike(left)
        case BinOp(Literal(1), "*", right) => simplifyMike(right)

        case BinOp(left, "+", Literal(0)) => simplifyMike(left)
        case BinOp(Literal(0), "+", right) => simplifyMike(right)

        case BinOp(left, "-", Literal(0)) => simplifyMike(left)

        case BinOp(left, "*", Literal(0)) => Literal(0)
        case BinOp(Literal(0), "*", right) => Literal(0)

        case BinOp(left, "*", right) => BinOp(simplifyMike(left), "*", simplifyMike(right))
        case BinOp(left, "-", right) => BinOp(simplifyMike(left), "-", simplifyMike(right))
        case BinOp(left, "+", right) => BinOp(simplifyMike(left), "+", simplifyMike(right))

        case Literal(value) => Literal(value)
        case Variable(name) => Variable(name)
    }

    // I didn't realize this was needed and coppied it from the autohrs solution
    if(res == expr) res
    else simplifyMike(res)
}

def simplify(expr: Expr): Expr = simplifyMike(expr)

val example1 = BinOp(Literal(1), "+", Literal(1))
println("strinified " + stringify(example1))
println("simplified " + stringify(simplify(example1)))

val example1_1 = BinOp(Literal(1), "-", Literal(1))
println("strinified " + stringify(example1_1))
println("simplified " + stringify(simplify(example1_1)))

val example1_2 = BinOp(Literal(4), "*", Literal(5))
println("strinified " + stringify(example1_2))
println("simplified " + stringify(simplify(example1_2)))

println()

val example2 = BinOp(BinOp(Literal(1), "+", Literal(1)), "*", Variable("x"))
println("stringified " + stringify(example2))
println("simplified " + stringify(simplify(example2)))

println()

val example3 = BinOp(
  BinOp(Literal(2), "-", Literal(1)),
  "*",
  Variable("x")
)
println("stringified " + stringify(example3))
println("simplified " + stringify(simplify(example3)))

println()

val example4 = BinOp(
  BinOp(BinOp(Literal(1), "+", (Literal(1))), "*", Variable("y")),
  "+",
  BinOp(BinOp(Literal(1), "-", (Literal(1))), "*", Variable("x"))
)
println("stringified " + stringify(example4))
println("simplified " + stringify(simplify(example4)))

markProblem(2)

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
    // I wanted this to work, but it doesn't for the general case. It doesn't count the nested [] correctly. I need to use a stack or something
    implicit def ParseSeq[T](implicit p: StrParser[T]) = new StrParser[Seq[T]]{
        def stripBrackets(s: String): Seq[String] = s match {
            case s"[$middle]" => stripBrackets(middle)
            case s"$left,$right" => stripBrackets(left) ++ stripBrackets(right)
            case other => Seq(other)
        }
        def parse(s: String) = {
            stripBrackets(s).map(p.parse)
        }
    }
}
def parseFromString[T](s: String)(implicit parser: StrParser[T]) = {
    parser.parse(s)
}

var aInt = parseFromString[Int]("77")
println(aInt.toString())
var aBool = parseFromString[Boolean]("true")
println(aBool.toString())
var aDouble = parseFromString[Double]("7.777")
println(aDouble.toString())


var someBools = parseFromString[Seq[Boolean]]("true,false,true")
println(someBools.toString())
var someInts = parseFromString[Seq[Int]]("111,222,333")
println(someInts.toString())
var someDoubles = parseFromString[Seq[Double]]("1.11,2.22,3.33")
println(someDoubles.toString())

var someSquareBools = parseFromString[Seq[Boolean]]("[true,false,true]")
println(someSquareBools.toString())

//var some3LayerStuff = parseFromString[Seq[(Seq[Int], Seq[Boolean])]]( 
    //"[[[1],[true]],[[2,3],[false,true]],[[4,5,6],[false,true,false]]]"
//)
//println(some3LayerStuff.toString())

markProblem(3)

trait StringWriter[T]{ def write(element: T): String }
object StringWriter{
    implicit object WriteInt extends StringWriter[Int]{
        def write(element: Int) = element.toString
    }
    implicit object WriteBoolean extends StringWriter[Boolean]{
        def write(element: Boolean) = element.toString
    }
    implicit object WriteDouble extends StringWriter[Double]{
        def write(element: Double) = element.toString
    }
    implicit def WriteSequence[T](implicit w: StringWriter[T]) = new StringWriter[Seq[T]]{
        def write(element: Seq[T]) = element.map(w.write).mkString("[",",","]")
    }
    implicit def WriteTuple[T, V](implicit w1: StringWriter[T], w2: StringWriter[V]) =
        new StringWriter[(T, V)]{
            def write(t: (T, V)) = {
                val (left, right) = t
                "[" + w1.write(left) + "," + w2.write(right) + "]"
            }
        }
}
def writeToString[T](element: T)(implicit w: StringWriter[T]) = {
    w.write(element)
}

println(writeToString(Seq(true, false, true)))
println(writeToString[Seq[(Seq[Int], Seq[Boolean])]](
    Seq(
        (Seq(1), Seq(true)),
        (Seq(2, 3), Seq(false, true)),
        (Seq(4, 5, 6), Seq(false, true, false))
        )
    ))
println(writeToString(
    Seq(
        (Seq(1), Seq((true, 0.5))),
        (Seq(2, 3), Seq((false, 1.5), (true, 2.5)))
    )
))