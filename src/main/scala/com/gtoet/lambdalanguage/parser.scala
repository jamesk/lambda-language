package com.gtoet.lambdalanguage

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  override def skipWhitespace = false
  override val whiteSpace = "[\t\r\f]+".r
  //override def skipWhitespace = true
  //override val whiteSpace = "[ \t\r\f]+".r

  /*
  def lambda: Parser[Unit] = {
    "\\" ^^ { _ => () }
  }

  def dot: Parser[Unit] = {
    "." ^^ { _ => () }
  }

  def space: Parser[Unit] = {
    " " ^^ { _ => () }
  }*/

  def variable: Parser[Variable] = {
    "[a-z0-9_]+".r ^^ { str =>
      if (str.length == 0)
        println("got zero length some how")

      Variable(str)
    }
  }

  def abstraction: Parser[Abstraction] = {
    ("\\" ~ variable ~ "." ~ term) ^^ {
      case _ ~ v ~ _ ~ t => Abstraction(v, t)
    }
  }

  def application: Parser[Application] = {
    ("(" ~ term ~ " " ~ term ~ ")") ^^ {
      case _ ~ t1 ~ _ ~ t2 ~ _ => Application(t1, t2)
    }
  }

  def term: Parser[Term] = {
    abstraction | variable | application
  }

  def apply(code: String): Either[String, Term] = {
    parse(term, code) match {
      case NoSuccess(msg, _) => Left(msg)
      case Success(result, next) => Right(result)
    }
  }
}