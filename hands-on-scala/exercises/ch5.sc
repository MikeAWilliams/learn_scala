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