package com.gtoet.lambdalanguage

object Interpreter {
  def fresh(t: Term, v: Variable): Boolean = !free(t).contains(v)

  def free(t: Term): Set[Variable] = {
    t match {
      case v: Variable => Set(v)
      case Application(t1, t2) => free(t1) ++ free(t2)
      case Abstraction(v, t1) => free(t1) - v
    }
  }

  def substitution(t: Term, from: Variable, to: Term): Term = {
    t match {
      case v: Variable if v == from => to
      case v: Variable if v != from => v
      case Application(t1, t2) => Application(substitution(t1, from, to), substitution(t2, from, to))
      case Abstraction(v, _) if v == from => t
      case Abstraction(v, t1) if v != from && fresh(to, v) => Abstraction(v, substitution(t1, from, to))
      case Abstraction(v, t1) if v != from && !fresh(to, v) => throw new NotImplementedError("auto alpha substitution not implemented yet")
      case _ => throw new RuntimeException("shouldn't be possible")
    }
  }


  def betaReduction(t: Term): Term = {
    t match {
      case Application(Abstraction(v, t1), t2: Term) => substitution(t1, v, t2) //reduce head position
      case Application(t1, t2) =>
        val next = betaReduction(t1)
        if (t1 != next) {
          Application(next, t2)
        } else {
          Application(t1, betaReduction(t2))
        }
      case Abstraction(v, t) => Abstraction(v, betaReduction(t))
      case v: Variable => v
    }
  }

  def betaNormalForm(t: Term): Term = {
    val next = betaReduction(t)
    println(next)
    if (next == t) {
      t
    } else {
      betaNormalForm(next)
    }
  }
}

object Prelude {
  val ident = Abstraction(Variable("x"), Variable("x"))
  //Natural numbers
  /*
  val n0 = Abstraction(Variable("f"), Abstraction(Variable("x"), Variable("x")))
  val succ = Abstraction(Variable("n"), Abstraction(Variable("f"), Abstraction(Variable("x"), Application(Variable("f"), Application(Application(Variable("n"), Variable("f")), Variable("x"))))))
  val sub = Abstraction(Variable("m"), Abstraction(Variable("n"), Application(Application(Variable("n"), pred), Variable("m"))))

  val n1 = Interpreter.betaNormalForm(Application(succ, n0))
  val n2 = Interpreter.betaNormalForm(Application(succ, n1))
  println("natural 2")
  println(n2)
  println("------")
  val n3 = Interpreter.betaNormalForm(Application(succ, n2))
  val n4 = Interpreter.betaNormalForm(Application(succ, n3))
  val n5 = Interpreter.betaNormalForm(Application(succ, n4))
*/
  //Logic
  val truel = Abstraction(Variable("x"), Abstraction(Variable("y"), Variable("x")))
  val falsel = Abstraction(Variable("x"), Abstraction(Variable("y"), Variable("y")))
  val and = Abstraction(Variable("p"), Abstraction(Variable("q"), Application(Application(Variable("p"), Variable("q")), Variable("p"))))
  val or = Abstraction(Variable("p"), Abstraction(Variable("q"), Application(Application(Variable("p"), Variable("p")), Variable("q"))))
  val not = Abstraction(Variable("p"), Application(Application(Variable("p"), falsel), truel))
  val ifthenelse = Abstraction(Variable("p"), Abstraction(Variable("a"), Abstraction(Variable("b"), Application(Application(Variable("p"), Variable("a")), Variable("b")))))
  //Logic - numbers
  val iszero = Abstraction(Variable("n"), Application(Application(Variable("n"), Abstraction(Variable("x"), falsel)), truel))
  //val leq = Abstraction(Variable("m"), Abstraction(Variable("n"), Application(iszero, Application(Application(sub, Variable("m")), Variable("n")))))
}

object main extends App {
  import Prelude._

  val trueAndTrue = Application(Application(and, truel), truel)
  val iftruethenident = Application(Application(Application(ifthenelse, truel), ident), falsel)
  val iffalsethenidentelsefalse = Application(Application(Application(ifthenelse, falsel), ident), falsel)

  println("True And True")
  println(trueAndTrue)
  println(Interpreter.betaNormalForm(trueAndTrue))

  println("If true then ident else false")
  println(iftruethenident)
  println(Interpreter.betaNormalForm(iftruethenident))
  println("If false then ident else false")
  println(iffalsethenidentelsefalse)
  println(Interpreter.betaNormalForm(iffalsethenidentelsefalse))

  /*
  (((λp.(λa.(λb.(((p a) b)))) λx.(λy.(y))) λx.(x)) λx.(λy.(y)))


  //(λb.(((λx.(λy.(x)) λx.(x)) b)) λx.(λy.(y)))
  val mid = Application(Abstraction(Variable("b"), Application(Application(truel, ident), Variable("b"))), falsel)
  println("mid")
  println(mid)
  println(Interpreter.betaNormalForm(mid))*/
}
