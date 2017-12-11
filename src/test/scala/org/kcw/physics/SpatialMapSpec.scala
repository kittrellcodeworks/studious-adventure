package org.kcw.physics

import org.kcw.physics.SpatialMap.SpatialMapContext
import org.scalatest._

class SpatialMapSpec extends WordSpec with Matchers {

  val obj0 = new GenericSpatialObject(BoundingBox(Point(1,1),Point(5,5)))
  val obj1 = new GenericSpatialObject(BoundingBox(Point(50,50),Point(60,60)))
  val obj2 = new GenericSpatialObject(BoundingBox(Point(25,25),Point(35,35)))
  val obj3 = new GenericSpatialObject(BoundingBox(Point(50,50),Point(55,55)))
  val obj4 = new GenericSpatialObject(Point(5,5))
  val obj5 = new GenericSpatialObject(Point(56,56))
  val obj6 = new GenericSpatialObject(Point(100,100))
  val obj7 = new GenericSpatialObject(Point(75,10))
  val obj8 = new GenericSpatialObject(Point(10,120))
  val obj9 = new GenericSpatialObject(Point(0,0))

  val sm = SpatialMap(SpatialMapContext(100, 100, 20), obj0, obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9)

  "SpatialMapContext" should {
    "correctly initialize" in {
      sm.ctx.bounds shouldBe BoundingBox(Point(0,0), Point(100,100))
      sm.ctx.bucketCount shouldBe 25
      sm.ctx.bucketsAcrossX shouldBe 5
      sm.ctx.bucketsAcrossY shouldBe 5
      sm.ctx.bucketWidth shouldBe 20.0 +- 0.25
      sm.ctx.bucketHeight shouldBe 20.0 +- 0.25
    }

    "correctly hash points to their respective grid buckets" in {
      sm.ctx hash Point(5,5) shouldBe 0
      sm.ctx hash Point(25,5) shouldBe 1
      sm.ctx hash Point(45,5) shouldBe 2
      sm.ctx hash Point(65,5) shouldBe 3
      sm.ctx hash Point(85,5) shouldBe 4
      sm.ctx hash Point(5,25) shouldBe 5
      sm.ctx hash Point(25,25) shouldBe 6
      sm.ctx hash Point(45,25) shouldBe 7
      sm.ctx hash Point(65,25) shouldBe 8
      sm.ctx hash Point(85,25) shouldBe 9
      sm.ctx hash Point(5,45) shouldBe 10
      sm.ctx hash Point(25,45) shouldBe 11
      sm.ctx hash Point(45,45) shouldBe 12
      sm.ctx hash Point(65,45) shouldBe 13
      sm.ctx hash Point(85,45) shouldBe 14
      sm.ctx hash Point(5,65) shouldBe 15
      sm.ctx hash Point(25,65) shouldBe 16
      sm.ctx hash Point(45,65) shouldBe 17
      sm.ctx hash Point(65,65) shouldBe 18
      sm.ctx hash Point(85,65) shouldBe 19
      sm.ctx hash Point(5,85) shouldBe 20
      sm.ctx hash Point(25,85) shouldBe 21
      sm.ctx hash Point(45,85) shouldBe 22
      sm.ctx hash Point(65,85) shouldBe 23
      sm.ctx hash Point(85,85) shouldBe 24
    }

    "correctly hash long rectangles" in {
      sm.ctx hash BoundingBox(Point(5,5),Point(10,95)) should contain theSameElementsAs Set(0, 5, 10, 15, 20)
      sm.ctx hash BoundingBox(Point(5,5),Point(95,10)) should contain theSameElementsAs Set(0, 1, 2, 3, 4)
    }

    "correctly hash rectangles overlapping the edge of the space" in {
      sm.ctx hash BoundingBox(Point(-5,-5),Point(5,5)) should contain theSameElementsAs Set(0)
      sm.ctx hash BoundingBox(Point(95,45),Point(205,55)) should contain theSameElementsAs Set(14)
    }

    "correctly hash rectangles containing the entire space" in {
      sm.ctx hash BoundingBox(Point(-1005,-25),Point(105,5000)) should contain theSameElementsAs Set(
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
    }
  }

  "SpatialMap" should {
    "detect out of bounds" in {
      sm.outOfBounds should contain theSameElementsAs Set(obj8)
    }

    "create the correct number of hash buckets" in {
      sm.size shouldBe 25
    }

    "detect collisions" in {
      sm.objectsCollidingWith(obj0) should contain theSameElementsAs Set(obj4)

      sm.objectsCollidingWith(obj1) should contain theSameElementsAs Set(obj3, obj5)

      sm.objectsCollidingWith(obj2) shouldBe empty

      sm.objectsCollidingWith(obj3) should contain theSameElementsAs Set(obj1)

      sm.objectsCollidingWith(obj4) should contain theSameElementsAs Set(obj0)

      sm.objectsCollidingWith(obj5) should contain theSameElementsAs Set(obj1)

      sm.objectsCollidingWith(obj6) shouldBe empty
      sm.objectsCollidingWith(obj7) shouldBe empty
      sm.objectsCollidingWith(obj8) shouldBe empty
      sm.objectsCollidingWith(obj9) shouldBe empty
    }

    "not detect collisions outside of the map bounds" in {
      val c = Circle(Point(11, 120), 3)
      // validate that c intercests an oob point
      c.intersects(obj8.shape) shouldBe true
      sm.objectsCollidingWith(c).size shouldBe 0
    }
  }

}
