package com.gtoet.lambdalanguage

object AstConstants {
  val Lambda = '\\'
  val Dot = '.'
  val LeftParen = '('
  val RightParen = ')'
}

sealed trait Term

case class Variable(name: String) extends Term {
  override def toString: String = name
}

//case class Constant(name: String) extends Term

case class Application(t1: Term, t2: Term) extends Term {
  override def toString: String = s"($t1 $t2)"
}

case class Abstraction(variable: Variable, t: Term) extends Term {
  override def toString: String = s"λ${variable}.($t)"
}