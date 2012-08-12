package GrowCode
import scala.collection.mutable.{Map, Stack => MStack, Set => MSet, Queue => MQueue}
import SlimGraph._
import scala.collection.mutable.OpenHashMap
import scala.collection.mutable.HashSet

import no.uib.cipr.matrix.sparse.{ FlexCompRowMatrix => SparseRowMat }

import scala.util._
import scalala.tensor.dense._
import scalala.library.Statistics._
import RichImplicits._
import scala.math._


object SlimGraphAlgorithms {
  def printProperties[N <% Ordered[N], E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E] ](
    graph: G with SimpleSlimGraphLike[N,E]
    ) {
    println("# Nodes: "+graph.order+" # Edges: "+graph.size)
    //println("Path length dist\t"+pathLengthDist(graph).toArray.mkString(","))
    println("Shape (deg dist)\t"+shape(graph).toArray.mkString(","))
    //println("LaplacianDist\t"+lch(laplacian(graph)).toArray.mkString(","))
    //println("EigenvectorCent\t"+ech(eigenvectorCentrality(graph)).toArray.mkString(","))
    //println("Effective diameter\t"+effectiveDiameter(graph, 0.9))
    //println("Char path length\t"+characteristicPathLength(graph))
    println("Assortativity\t"+assortativity(graph))
    //println("Clustering\t"+clustering(graph))
    println("AVG Clustering\t"+avgClustering(graph))
  }


  def laplacian[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
    graph: G with SimpleSlimGraphLike[N,E]
    ) = {
      val lapCalc = new LaplacianCalculator(graph)
      val (lap, vols) = lapCalc.compute
      val N = graph.order

      import org.ejml.ops._
      import org.ejml.data.DenseMatrix64F
      import org.ejml.alg.dense.decomposition._

      val denseL = new DenseMatrix64F(N, N)
      val it = lap.iterator
      while( it.hasNext ) {
        val ent = it.next
        denseL.unsafe_set(ent.row, ent.column, ent.get)
      }

      // Perform the eigendecomposition
      val decomposer = DecompositionFactory.eigSymm(N, true)
      var devals = Array.fill(N-1)(0.0)
      try {
        decomposer.decompose(denseL)
        // Sort the indices
        val sortedInds = (0 until N).map{ i => (decomposer.getEigenvalue(i).real,i) }.toArray.sorted.map{ vi => vi._2 }
        // Get the eigenvalues and eigenvectors in the sorted order
        devals = (0 until N).map{ i => decomposer.getEigenvalue( sortedInds(i) ).real }.toArray
      } catch {
        case e: Exception => ()
      }
      devals
  }

  def lch(laplacian: Array[Double]) = {
    new Histogram(laplacian, Some((0.0,2.0)), 20, true).hist
  }

  def ech(eigCentrality: Array[Double]) = {
    new Histogram(eigCentrality, Some((0.0,1.0)), 20, true).hist
  }

  def diameter[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
    graph: G with SimpleSlimGraphLike[N,E]): Int = {
    val sp = new ShortestPathOperationsSG(graph)
    var maxLength = 0
    graph.vertices.foreach{ v =>
      sp.singleSourceShortestPaths(v)
      val maxDist = sp.dists.values.filter{ x=> x < Int.MaxValue }.max
      maxLength = scala.math.max(maxLength, maxDist)
    }
    maxLength
  }

  def characteristicPathLength[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
      graph: G with SimpleSlimGraphLike[N,E]): Double = {
    val sp = new ShortestPathOperationsSG(graph)
    var avgLength = 0.0
    var pathLengths = List[Double]()
    graph.vertices.foreach{ v =>
      sp.singleSourceShortestPaths(v)
      pathLengths = sp.dists.values.filter{ x=> x < Int.MaxValue }.map{_.toDouble}.toList ::: pathLengths
    }
    pathLengths.sum.toFloat/pathLengths.size
  }

  def pathLengthDist[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
      graph: G with SimpleSlimGraphLike[N,E]) = {
    val sp = new ShortestPathOperationsSG(graph)
    var avgLength = 0.0
    var pathLengths = List[Double]()
    graph.vertices.foreach{ v =>
      sp.singleSourceShortestPaths(v)
      pathLengths = sp.dists.values.filter{  x=> x < Int.MaxValue }.map{_.toDouble}.toList ::: pathLengths
    }
    new Histogram(pathLengths, Some((0, 40)), 40, true).hist
  }


  def effectiveDiameter[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
      graph: G with SimpleSlimGraphLike[N,E], cutoff: Double): Double = {
    if (graph.size == 0) { return 0.0 }
    /*
    val sp = new ShortestPathOperations(graph)
    var allPairsSP = List[Int]()
    graph.nodes.foreach{ v =>
      sp.singleSourceShortestPaths(v.value)
      allPairsSP = allPairsSP ++ sp.dists.values.filter{ x=> x < Int.MaxValue && x > 0 }.toList
    }
    */
    val numSamp = min( graph.order, 200 )
    val allPairsSP = graph.vertices.sample(numSamp).par.iterator.flatMap{ v =>
    //val allPairsSP = graph.nodes.par.iterator.flatMap{ v =>
      val sp = new ShortestPathOperationsSG(graph)
      sp.singleSourceShortestPaths(v).values.filter{ x=> x < Int.MaxValue && x > 0 }.toList
    }

    val m = OpenHashMap[Int, Double]()
    val c = OpenHashMap[Int, Double]()
    allPairsSP.foreach{ sp=> m(sp) = m.getOrElse(sp,0.0) + 1.0 }
    var cumulative = 0.0
    m.keys.toSeq.sorted.foreach{ k=>
      cumulative += m(k)
      c(k) = cumulative
    }
    m.keys.foreach{ k=> c(k) /= cumulative }
    var effDiam = c.keys.toSeq.sorted.find { k => c(k) >= cutoff }.get
    if (effDiam-1 == 0) { return 0.0 }
    val x1 = effDiam-1
    val y1 = c(x1)
    val dy = (c(effDiam)-c(effDiam-1))
    (dy*x1 + cutoff - y1)/dy
  }

  def eigenvectorCentrality[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
    graph: G with SimpleSlimGraphLike[N,E]) = {
    val adjMat = new SparseRowMat( graph.order, graph.order )
    val nodeIndMap = graph.vertices.zipWithIndex.toMap
    graph.vertices.foreach{ u =>
      val ui = nodeIndMap(u)
      graph.neighborVertices(u).foreach{ v =>
        val vi = nodeIndMap(v)
        adjMat.set(ui,vi,1)
      }
    }

    val pe = PowerIteration.leadingEig(adjMat, 50)
    pe.getData
  }


  /**  Subsample the source graph at a level of frac
    *
    *
    */
  def subSample[E <: SimpleEdgeLike[Int], G <: SimpleSlimGraphLike[Int,E]](
    source: G with SimpleSlimGraphLike[Int,E],
    makeEdge: (Int,Int) => E,
    frac: Double )(implicit mf: Manifest[G], ev: Int => Ordered[Int] ) = {


    val ctor = mf.erasure.getConstructor(classOf[Int => Ordered[Int]])
    val graphc = ctor.newInstance(ev).asInstanceOf[G]


    val random = new Random(System.currentTimeMillis)
    val target = ctor.newInstance(ev).asInstanceOf[G]
    val p = 0.7
    val geom = new GeometricRV(p)

    // Do a single step of the forest fire model
    def burn(sourceVert: Int) {
      // Only want to visit vertices once per step
      val seen = MSet.empty[Int]
      val toVisit = MQueue.empty[Int]
      toVisit += sourceVert
      seen += sourceVert
      // until we've nobody left to visit
      while ( !toVisit.isEmpty ) {
        // get someone from the queue
        val otherVert = toVisit.dequeue()
        // chose which of their neighbors to "burn"
        //val otherVert = source.get(otherVertId)
        val ng = geom.next
        val x = min(source.degree(otherVert), ng)
        var burnedNeighbors = List.empty[Int]
        source.neighborVertices(otherVert).toSeq.sample(x).foreach{ x =>
          if( !seen.contains(x) ) {
            seen += x
            burnedNeighbors = x :: burnedNeighbors
          }
        }
        // add the new edges
        burnedNeighbors.foreach{ n =>
          target.insertEdge( makeEdge(otherVert, n) )
          toVisit += n
        }

      }
    }

    assert( frac < 1.0 )
    val targetOrder = (frac * source.order).toInt
    val usedSeeds = MSet.empty[Int]
    while( target.order < targetOrder ) {
      val seed = random.nextInt( source.order )
      if ( ! usedSeeds.contains(seed) ) {
        burn(seed)
        usedSeeds += seed
     }
    }
    target
  }

  def shape[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
    graph: G with SimpleSlimGraphLike[N,E] ) = {
    var ds = graph.vertices.toSeq.map({v => graph.degree(v)}).toArray
    if (ds.size < 1) {
      ds = Array.ofDim[Int](1)
    }
    val sumds = ds.max + 1
    val h = new Histogram( ds.map{ x => x.toDouble/sumds }, nbins=20, normed=true, drange=Some((0.0,1.0))  )
    //println("ORIGINIAL"+h.hist)
    (1 until h.hist.size).foreach{i=>h.hist(i) += h.hist(i-1)}
    //println("CUMULATIVE"+h.hist)
    h.hist
  }

  def assortativity[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
  graph: G with SimpleSlimGraphLike[N,E]): Double = {

    val degreeSource = DenseVector.zeros[Double](2*graph.size+1)
    val degreeTarget = DenseVector.zeros[Double](2*graph.size+1)

    // Avoid degeneracies
    degreeSource(degreeSource.size-1) = .5
    degreeTarget(degreeTarget.size-1) = .5

    var i = 0
    graph.edges.foreach{ e =>
      degreeSource(i) = graph.degree(e.source)
      degreeTarget(i) = graph.degree(e.target)
      i+=1
    }
    graph.edges.foreach{ e =>
      // only count self loops once
      if (e.source != e.target) {
      degreeSource(i) = graph.degree(e.target)
      degreeTarget(i) = graph.degree(e.source)
      i+=1
      }
    }
    
    val cc = corrcoef(degreeSource(0 to i), degreeTarget(0 to i))
    if ( cc.isNaN ) { 0.0 } else { cc }
  }


  def avgClustering[N <% Ordered[N], E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
    graph: G with SimpleSlimGraphLike[N, E]): Double = {
  
    def clustering( u : N ) = {
        

        /*val nu = graph.neighborVertices(u).toSet &~ Set(u)
        
        numClosedTriples = nu.toSeq.map{ v => (nu & graph.neighborVertices(v).toSet).size }.sum*/
        var (numClosedTriples,nn) = (0,0)
        graph.neighbors(u).foreach{ case (v,e2) =>
          if ( v != u ) {
          nn += 1
          graph.neighbors(v).foreach{ case (w,e3) =>
             if( w > v && e2 != e3 && (graph.hasEdge(u,w) || graph.hasEdge(w,u)) ) {
                numClosedTriples += 1
            }
          }
        }
        }
        
      if ( nn > 1 ) { (2*numClosedTriples).toDouble/((nn*(nn-1))) } else{ 0.0 }
    }

    // For each vertex
    var nvert = 0
    graph.vertices.toSeq.map{ u =>
      nvert += 1
      clustering(u)
    }.sum / nvert

  }

  def clustering[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
    graph: G with SimpleSlimGraphLike[N,E]): Double = {
    var numClosedTriples = 0
    var numConnectedTriples = 1
    graph.vertices.foreach{ x=>
      graph.neighborVertices(x).toSeq.combinations(2).foreach{ case Seq(v,w) =>
        numConnectedTriples += 1
        if( graph.edgeOption(v,w).isDefined ) { numClosedTriples += 1 }
      }
    }
    numClosedTriples.toDouble/numConnectedTriples
  }

}

class ShortestPathOperationsSG[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E] ](
  graph: G with SimpleSlimGraphLike[N,E]) {

  var dists = OpenHashMap.empty[N, Int]
  val seen = HashSet.empty[N]

  def singleSourceShortestPaths(v: N) = {
    graph.vertices.foreach { v=> dists(v) = Int.MaxValue }
    // The first level is 0
    var level = 0
    var nextLevel = HashSet.empty[N]
    nextLevel += v
    // While there are vertices yet to be visited
    while ( nextLevel.size > 0 ) {
      // The previous round's nextLevel is this round's level
      val thisLevel = nextLevel
      // We create a new set to hold the next round's vertices
      nextLevel = HashSet.empty[N]
      // For each vertex in this level
      thisLevel.foreach{ v =>
        // If we haven't seen this vertex yet
        if ( ! (dists(v) < Int.MaxValue) ) {
          // Set it's distance from the root
          dists(v) = level
          // Add it's neighbors to the hash for the next level
          graph.neighborVertices(v).foreach{ u => nextLevel.add(u) }
        }
      }
      level += 1
    }
    dists
  }

}