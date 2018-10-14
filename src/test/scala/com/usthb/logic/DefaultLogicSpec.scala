package com.usthb.logic

import com.usthb.logic.propositional.Literals._
import org.scalatest.{FunSpec, Matchers}
import com.usthb.logic.defaultlogic.Default._
import com.usthb.logic.defaultlogic.Theory

import scala.collection.Set

class DefaultLogicSpec extends FunSpec with Matchers {
  describe("A theory") {
    describe("extentions method") {
      it("should return all possible extentions") {
        val world = Set(A)
        val defaults = Set(
          (A * B) / C,
          (A * !C) / !B
        )

        val delta = Theory(world, defaults)

        //delta.extentions shouldEqual Set(Set(A, C), Set(A, !B))
      }
    }
  }
}
