package GrowCode
import SlimGraph._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.mutable.{Graph => MGraph}
import scala.collection.mutable.{ Set => MSet, Queue => MQueue }
import scala.util._
import scala.math._
import scalala.library.Plotting._

object RichImplicits {
  implicit def wrapIndexable[A](seq: IndexedSeq[A]) = new RichIndexable(seq)
  // An iterable of type A for which we implicitly pass in ClassManifest to
  // obtain a type at run time (this is because of type erasure).  S is viewable as an
  // iterable object.
  implicit def wrapIterable[A, S[A] <: Iterable[A]](seq: S[A]) =
    new RichIterable(seq)

  class RichIndexable[A](val seq: IndexedSeq[A]) {
    val random = scala.util.Random
    def choice() = { seq(random.nextInt(seq.size)) }
  }

  class RichIterable[A, S[A] <: Iterable[A]](val seq: S[A]) {
    val b = seq.genericBuilder[A]
    val random = scala.util.Random
    def sample(k: Int) = {
      val n = seq.size
      // TODO: If you want more than half the elements, ...
      assert (k <= n, { println("k = "+k+" > n = "+n) } )
      var i = 0
      var numerator = k
      val it = seq.iterator
      while (numerator > 0) {
        val denominator = n - i
        val prob = numerator.toDouble/denominator
        val x = it.next
        if (random.nextDouble < prob) {
          b += x
          numerator -= 1
        }
        i += 1
      }
      b.result
    }
    def choice() = { sample(1).head }
  }
}

class GeometricRV(val p: Double) {
    val random = new Random(System.currentTimeMillis)
    // removed the +1 below (we should be able to visit 0 more vertices)
    def next = { (log(random.nextDouble)/log(1-p)).toInt }
}

object GrowthModels {

  import RichImplicits._

  type EdgeT = UndirectedEdge[Int]
  type GraphT = SlimGraphUnDirected[Int,EdgeT]

  def ForestFire(n: Int, p: Double, f: (GraphT) => Unit, samp: Int): GraphT = {
    val random = new Random(System.currentTimeMillis)
    val graph = new SlimGraphUnDirected[Int,EdgeT]()
    graph.insertEdge( UndirectedEdge(0,1) )
    val geom = new GeometricRV(p)

    // Do a single step of the forest fire model
    def doStep(newVertId: Int) {
      // Only want to visit vertices once per step
      val seen = MSet.empty[Int]
      val toVisit = MQueue.empty[Int]
      val u = random.nextInt(graph.order)
      toVisit += u
      seen += u
      // until we've nobody left to visit
      while ( !toVisit.isEmpty ) {
        // get someone from the queue
        val otherVert = toVisit.dequeue()
        // chose which of their neighbors to "burn"
        //val otherVert = graph.get(otherVertId)
        val ng = geom.next
        val x = min(graph.degree(otherVert), ng)
        graph.neighborVertices(otherVert).toIterable.sample(x).foreach{ x =>
          if( !seen.contains(x) ) {
            seen += x.value
            toVisit += x.value
          }
        }
        // connect to the original vertex
        graph.insertEdge( UndirectedEdge(otherVert,newVertId) )

      }
    }

    (2 until n).foreach { v =>
      doStep(v)
      if (v % samp == 0) { f(graph) }
    }

    graph
  }

  def DMC(n: Int, qmod: Double, qcon: Double): GraphT = {
    val random = new Random(System.currentTimeMillis)
    val graph = new SlimGraphUnDirected[Int,EdgeT]()
    graph.insertEdge(UndirectedEdge(0,1))

    (2 until n).foreach{ u =>
      val v = random.nextInt(graph.order)
      graph.insertVertex(u)

      // Make a copy of v
      graph.neighborVertices(v).foreach{ x=> graph.insertEdge(UndirectedEdge(u,x)) }

      // Remove edges via qmod
      graph.neighborVertices(u).foreach{ x=>
        if (random.nextDouble < qmod) {
          if (random.nextDouble < 0.5) { graph.removeEdge(UndirectedEdge(v,x)) }
          else { graph.removeEdge(u,x) }
        }
      }

      // Add edge via qcon
      if (random.nextDouble < qcon) { graph.insertEdge( UndirectedEdge(u,v) ) }
  
      if (graph.size > 3*n) { return graph }


    }
    graph
  }

}
