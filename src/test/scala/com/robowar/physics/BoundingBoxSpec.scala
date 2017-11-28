package com.robowar.physics

import org.scalatest._

class BoundingBoxSpec extends WordSpec with Matchers {

  val test = BoundingBox(Point(5, 5), Point(10,10))

  "BoundingBox" when {
    "evaluating another BoundingBox for collisions" should {
      "return false when the target is completely ouside" in {
        test intersects BoundingBox(Point(-5, -5), Point(-3, -3)) shouldBe false
      }
      "return true" when {
        "the target is completely inside" in {
          test intersects BoundingBox(Point(6, 6), Point(7, 7)) shouldBe true
        }
        "the target overlaps the bottom left" in {
          test intersects BoundingBox(Point(0, 0), Point(7, 7)) shouldBe true
        }
        "the target overlaps the bottom right" in {
          test intersects BoundingBox(Point(7, 0), Point(12, 7)) shouldBe true
        }
        "the target overlaps the top left" in {
          test intersects BoundingBox(Point(0, 7), Point(7, 12)) shouldBe true
        }
        "the target overlaps the top right" in {
          test intersects BoundingBox(Point(7, 7), Point(12, 12)) shouldBe true
        }
        "the target overlaps the top" in {
          test intersects BoundingBox(Point(0, 7), Point(12, 12)) shouldBe true
        }
        "the target overlaps the right" in {
          test intersects BoundingBox(Point(7, 0), Point(12, 12)) shouldBe true
        }
        "the target overlaps the bottom" in {
          test intersects BoundingBox(Point(0, 0), Point(12, 7)) shouldBe true
        }
        "the target overlaps the left" in {
          test intersects BoundingBox(Point(0, 0), Point(7, 12)) shouldBe true
        }
      }
    }
    "evaluating a Point for collisions" should {
      "return false when the target is outside" in {
        test intersects Point.zero shouldBe false
        test intersects Point(15, 15) shouldBe false
      }
      "return true when the target is inside" in {
        test intersects Point(7, 7) shouldBe true
        test intersects Point(5, 7) shouldBe true // on the edge
      }
    }
  }
}
