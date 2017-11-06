package com.gtoet.lambdalanguage

import com.gtoet.lambdalanguage.Prelude._
import org.scalatest.{FunSpec, Matchers}

class InterpreterTest extends FunSpec with Matchers {

  describe("betaNormalForm") {
    describe("for boolean logic") {
      it("should resolve true AND true to true") {
        Interpreter.betaNormalForm(Application(Application(and, truel), truel)) shouldBe truel
      }

      it("should resolve if true then ident to ident") {
        val ifTrueThenIdent = Application(Application(Application(ifthenelse, truel), ident), falsel)

        Interpreter.betaNormalForm(ifTrueThenIdent) shouldBe ident
      }

      it("should resolve if false then ident else false to false") {
        val ifFalseThenIdentElseFalse = Application(Application(Application(ifthenelse, falsel), ident), falsel)

        Interpreter.betaNormalForm(ifFalseThenIdentElseFalse) shouldBe falsel
      }
    }

  }

}
