package org.kcw.precalc

import org.scalatest._

class SineSpec extends WordSpec with Matchers {

  "unsafe_sin" should {
    "be 0 for 0 degrees" in {
      unsafe_sin(0) shouldBe 0D
    }

    "be 1 for 90 degrees" in {
      unsafe_sin(90) shouldBe 1.0D
    }

    "be 0 for 180 degrees" in {
      unsafe_sin(180) shouldBe 0D
    }

    "be -1 for 270 degrees" in {
      unsafe_sin(270) shouldBe -1.0D
    }
  }

}
