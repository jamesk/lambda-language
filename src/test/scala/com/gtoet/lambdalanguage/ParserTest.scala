package com.gtoet.lambdalanguage

import org.scalatest.{FunSpec, Inside, Matchers}
import com.gtoet.lambdalanguage.Prelude._

class ParserTest extends FunSpec with Matchers with Inside {
  describe("parser") {
    it("should not parse empty string") {
      Parser("") should matchPattern {
        case Left(_) =>
      }
    }

    it("should not parse illegal symbols") {
      Parser("\\x.$%%#$&&*") should matchPattern {
        case Left(_) =>
      }
    }

    it("should parse a variable") {
      inside(Parser("x")) {
        case Right(result) =>
          result shouldBe Variable("x")
      }
    }


    it("should parse ident") {
      inside(Parser("\\x.x")) {
        case Right(result) =>
          result shouldBe ident
      }
    }

    it("should parse application") {
      inside(Parser("(\\x.x \\x.x)")) {
        case Right(result) =>
          result shouldBe Application(ident, ident)
      }
    }
  }
}
