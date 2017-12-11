package org.kcw.physics

import org.scalatest._

class VectSpec extends WordSpec with Matchers {

  val v1 = Vect(3, 4) // 3-4-5
  val v2 = Vect(5, -12) // 5-12-13
  val v3 = Vect(-8, -15) // 8-15-17

  "A Vector" should {
    "add correctly" in {
      val a = Vect(1, 2) + Vect(2, 3)
      val b = Vect(-1, 2) + Vect(2, -3)
      a shouldBe Vect(3, 5)
      b shouldBe Vect(1, -1)
    }

    "subtract correctly" in {
      val a = Vect(1, 2) - Vect(2, 3)
      val b = Vect(-1, 2) - Vect(2, -3)
      a shouldBe Vect(-1, -1)
      b shouldBe Vect(-3, 5)
    }

    "calculate magnitude correctly" in {
      v1.magnitude shouldBe 5.0 +- tolerance
      v2.magnitude shouldBe 13.0 +- tolerance
      v3.magnitude shouldBe 17.0 +- tolerance
    }

    "project magnitude correctly" in {
      // perpendicular lines should have 0 projection
      Vect(100, 0).projectedMagnitude(Vect(0, -10)) shouldBe 0.0 +- tolerance

      // sign of the magnitude should be relative to the direction of the projection with respect to the target vector.
      v1.projectedMagnitude(Vect(1, 0)) shouldBe 3.0 +- tolerance
      v1.projectedMagnitude(Vect(-1, 0)) shouldBe -3.0 +- tolerance
      v3.projectedMagnitude(Vect(0, 1)) shouldBe -15.0 +- tolerance
      v3.projectedMagnitude(Vect(0, -1)) shouldBe 15.0 +- tolerance

      // test a projection on a non-axis vector
      Vect(10, 20).projectedMagnitude(Vect(-1, -1)) shouldBe -15 * math.sqrt(2) +- tolerance
    }

    "rotate clockwise" in {
      v1.rotate90cw shouldBe Vect(4, -3)
      v2.rotate90cw shouldBe Vect(-12, -5)
      v3.rotate90cw shouldBe Vect(-15, 8)
    }

    "rotate counter-clockwise" in {
      v1.rotate90ccw shouldBe Vect(-4, 3)
      v2.rotate90ccw shouldBe Vect(12, 5)
      v3.rotate90ccw shouldBe Vect(15, -8)
    }

    "project correctly" in {
      // perpendicular lines should have 0 projection
      Vect(100, 0).project(Vect(0, -10)) shouldBe Vect.zero

      val v2b = v2.rotate90cw
      v2.project(v2b) shouldBe Vect.zero

      v1.project(Vect(-1, 0)) shouldBe Vect(3, 0)

      Vect(10, 20).project(Vect(-1, -1)) shouldBe Vect(15, 15)
    }

  }

}
