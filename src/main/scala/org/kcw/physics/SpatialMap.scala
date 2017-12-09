package org.kcw.physics

import scala.collection.mutable
import SpatialMap._

object SpatialMap {

  case class SpatialMapContext(w: Int, h: Int, gridSize: Int) {

    val bounds = BoundingBox(Point.zero, Point(tolerance + h, tolerance + w))

    private[physics] val bucketsAcrossX: Int = w / gridSize
    private[physics] val bucketWidth: Double = (w + 1.0D) / bucketsAcrossX
    private[physics] val xHashFactor: Double = 1.0D / bucketWidth
    private[physics] val bucketsAcrossY: Int = h / gridSize
    private[physics] val bucketHeight: Double = (h + 1.0D) / bucketsAcrossY
    private[physics] val yHashFactor: Double = 1.0D / bucketHeight
    private[physics] val bucketCount: Int = (w / gridSize) * (h / gridSize) // NOT commutable due to remainder truncation.

    private[physics] def hash(p: Point): Int = {
      // TODO -- this isn't very robust - out-of-bounds points don't hash correctly.
      val hx = (p.x * xHashFactor).toInt
      val hy = (p.y * yHashFactor).toInt
      hx + (hy * bucketsAcrossX)
    }

    private[physics] def hash(b: BoundingBox): Set[Int] = { // NOTE: these results could be out of bounds, so filter them
      (bounds & b) match {
        case Some(rect) ⇒
          val min = hash(rect.min)
          val max = hash(rect.max)
          if (min == max) Set(min)
          else {
            val d = max - min
            if (d == 1 || d == bucketsAcrossX) Set(min, max)
            else if (d == bucketsAcrossX + 1) Set(min, min + 1, max - 1, max)
            else {
              // list the boxes between min and max, this is the most expensive option
              val cols = Range.inclusive(0, d % bucketsAcrossX)
              val rows = Range.inclusive(0, d / bucketsAcrossX)
              (for {r ← rows; c ← cols} yield min + (r * bucketsAcrossX) + c).toSet
            }
          }
        case None ⇒ Set.empty
      }
    }
  }

  // NOTE: gridSize should be larger than the diameter of most objects
  class SpatialMapBuilder[T <: SpatialObject] private(ctx: SpatialMapContext, from: Option[SpatialMap[T]] = None)
    extends mutable.ReusableBuilder[T, SpatialMap[T]] with mutable.Cloneable[SpatialMapBuilder[T]] {

    require(from.forall(_.ctx == ctx), "If a SpatialMap is provided to a builder, the SpatialMapContext should match!")

    def this(ctx: SpatialMapContext) = this(ctx, None)

    /**
      * For use cases such as overlaying moving objects over already-hashed "static" objects.
      *
      * WARNING: Re-adding previously hashed objects may lead to undesired results.
      *
      * @param existing An existing "base" SpatialMap
      */
    def this(existing: SpatialMap[T]) = this(existing.ctx, Some(existing))

    protected var table: Array[Set[T]] = from.map(_.table.toArray).getOrElse(Array.fill(ctx.bucketCount)(Set.empty))
    protected var entries: Map[T, Set[Int]] = from.map(_.entries).getOrElse(Map.empty)
    protected var oob: Set[T] = from.map(_.oob).getOrElse(Set.empty)

    override def +=(elem: T): SpatialMapBuilder.this.type = {
      val b = elem.shape.bounds
      val hs = ctx.hash(b) // .filter(tb.isDefinedAt)
      entries = entries.updated(elem, hs)
      if (hs.isEmpty) oob += elem
      else hs.foreach { table(_) +=  elem }
      this
    }

    override def clear(): Unit = {
      table = from.map(_.table.toArray).getOrElse(Array.fill(ctx.bucketCount)(Set.empty))
      entries = from.map(_.entries).getOrElse(Map.empty)
      oob = from.map(_.oob).getOrElse(Set.empty)
    }

    override def result(): SpatialMap[T] = new SpatialMap(ctx, entries, table, oob)

    def mark(): SpatialMapBuilder[T] = new SpatialMapBuilder(this.result())
  }

  def newBuilder[T <: SpatialObject](ctx: SpatialMapContext): mutable.Builder[T, SpatialMap[T]] =
    new SpatialMapBuilder[T](ctx)

  def apply[T <: SpatialObject](ctx: SpatialMapContext, obj: T*): SpatialMap[T] = {
    val b = newBuilder[T](ctx)
    for (o ← obj) b += o
    b.result
  }
}


class SpatialMap[T <: SpatialObject] private(val ctx: SpatialMapContext,
                         private val entries: Map[T, Set[Int]],
                         private val table: IndexedSeq[Set[T]],
                         // will not be included in collision detections
                         private val oob: Set[T]
                        ) {

  def size: Int = table.size

  override def toString: String = {
    table.zipWithIndex.map {
      case (es, i) ⇒
        val estr = es.map(_.shape.toString).mkString("[",",","]")
        val bx = i % ctx.bucketsAcrossX
        val by = i / ctx.bucketsAcrossX
        s"$i (${bx * ctx.bucketWidth}, ${by * ctx.bucketHeight}) => $estr"
    }.mkString("\n")
  }

  // TODO: real-time collision detection with velocities. -- this is unnecessary for now.

  def outOfBounds: Set[T] = oob

  def objectsInCell(i: Int): Set[T] = table(i)

  def locationOf(bounds: BoundingBox): Set[Int] = ctx hash bounds

  def locationOf(obj: T): Set[Int] =
    entries.getOrElse(obj, ctx.hash(obj.shape.bounds))

  def objectsNear(bounds: BoundingBox): Set[T] =
    locationOf(bounds) flatMap table

  def objectsNear(obj: T): Set[T] =
    locationOf(obj).flatMap(table(_) - obj)

  def objectsCollidingWith(obj: T): Iterable[T] =
    objectsNear(obj).filter(_.intersects(obj))

  def +(obj: T): SpatialMap[T] = {
    val newBounds = obj.shape.bounds
    val newHashes = ctx.hash(newBounds)
    val prevHashes = entries.getOrElse(obj, Set.empty)
    val newOoB = if (newHashes.isEmpty) oob + obj else oob - obj
    val newTable = IndexedSeq.tabulate(ctx.bucketCount) { i ⇒
      val old = table(i)
      val wasInOld = prevHashes(i)
      val isInNew = newHashes(i)
      if (isInNew && !wasInOld) old + obj
      else if (!isInNew && wasInOld) old - obj
      else old
    }
    val newEntries = entries + (obj → newHashes)
    new SpatialMap(ctx, newEntries, newTable, newOoB)
  }

  def - (obj: T): SpatialMap[T] = {
    val prevHashes = entries.getOrElse(obj, Set.empty)
    val newTable = IndexedSeq.tabulate(ctx.bucketCount) { i ⇒
      val old = table(i)
      if (prevHashes(i)) old - obj
      else old
    }
    new SpatialMap(ctx, entries - obj, newTable, oob - obj)
  }

  def asTemplate(): SpatialMapBuilder[T] = new SpatialMapBuilder(this)
}
