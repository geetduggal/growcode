import scala.collection.mutable.{ OpenHashMap => Map, HashSet }
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.Ordered

package SlimGraph {

  trait SimpleEdgeLike[N] {
    val source: N
    val target: N
  }

  trait UnorderedEdgeLike[N] extends SimpleEdgeLike[N] {
    override def hashCode() = { source.hashCode + target.hashCode }
    override def equals(other: Any) = other match {
      case o: SimpleEdgeLike[N] => {
        (o.source == source && o.target == target)
      }
      case _ => false
    }
  }

  trait OrderedEdgeLike[N] extends SimpleEdgeLike[N] {
    override def hashCode() = { source.hashCode + 2 * target.hashCode }
    override def equals(other: Any) = other match {
      case o: SimpleEdgeLike[N] => {
        (o.source == source && o.target == target)
      }
      case _ => false
    }
  }

  trait WeightedEdgeLike[W] {
    var weight: W
  }

  object UndirectedEdge {
    def factory[N <% Ordered[N]] = { (u: N, v: N) => UndirectedEdge(u, v) }
  }

  object DirectedEdge {
    def factory[N] = { (u: N, v: N) => DirectedEdge(u, v) }
  }

  object WeightedUndirectedEdge {
    def factory[N <% Ordered[N], W] = { (u: N, v: N, w: W) => WeightedUndirectedEdge(u, v, w) }
    def factoryWithDefault[N <% Ordered[N], W](w: W) = { (u: N, v: N) => WeightedUndirectedEdge(u, v, w) }
  }

  object WeightedDirectedEdge {
    def factory[N, W] = { (u: N, v: N, w: W) => WeightedDirectedEdge(u, v, w) }
    def factoryWithDefault[N, W](w: W) = { (u: N, v: N) => WeightedDirectedEdge(u, v, w) }
  }

  case class UndirectedEdge[N <% Ordered[N]](u: N, v: N) extends UnorderedEdgeLike[N] {
    val (source, target) = if (u > v) { (v, u) } else { (u, v) }
  }

  case class WeightedUndirectedEdge[N <% Ordered[N], WT](u: N, v: N, var weight: WT) extends UnorderedEdgeLike[N] with WeightedEdgeLike[WT] {
    val (source, target) = if (u > v) { (v, u) } else { (u, v) }
  }

  case class DirectedEdge[N](source: N, target: N) extends OrderedEdgeLike[N]

  case class WeightedDirectedEdge[N, WT](source: N, target: N, var weight: WT) extends OrderedEdgeLike[N] with WeightedEdgeLike[WT]

  trait EdgeChangedEvent[N, E <: SimpleEdgeLike[N]] {
    val e: E
  }

  case class EdgeAddedEvent[N, E <: SimpleEdgeLike[N]](e: E) extends EdgeChangedEvent[N, E]
  case class EdgeRemovedEvent[N, E <: SimpleEdgeLike[N]](e: E) extends EdgeChangedEvent[N, E]

  trait VertexChangedEvent[N] {
    val v: N
  }

  case class VertexAddedEvent[N](v: N) extends VertexChangedEvent[N]
  case class VertexRemovedEvent[N](v: N) extends VertexChangedEvent[N]

  trait ObservableGraph[N, E <: SimpleEdgeLike[N]] {
    type EdgeHandle
    type VertexHandle
    type EdgeCallback = (EdgeChangedEvent[N, E]) => Unit
    type VertexCallback = (VertexChangedEvent[N]) => Unit

    val edgeCallbacks = Map.empty[EdgeHandle, EdgeCallback]
    val vertexCallbacks = Map.empty[VertexHandle, VertexCallback]

    def observeEdge(callback: EdgeCallback): EdgeHandle = {
      val handle = createEdgeHandle(callback)
      edgeCallbacks += (handle -> callback)
      handle
    }

    def observeVertex(callback: VertexCallback): VertexHandle = {
      val handle = createVertexHandle(callback)
      vertexCallbacks += (handle -> callback)
      handle
    }

    def unobserveEdge(handle: EdgeHandle): Unit = {
      edgeCallbacks -= handle
    }
    def unobserveVertex(handle: VertexHandle): Unit = {
      vertexCallbacks -= handle
    }

    protected def createEdgeHandle(callback: (EdgeChangedEvent[N, E]) => Unit): EdgeHandle
    protected def createVertexHandle(callback: (VertexChangedEvent[N]) => Unit): VertexHandle
    protected def notifyEdgeListeners(e: EdgeChangedEvent[N, E]): Unit = for (callback <- edgeCallbacks.values) callback(e)
    protected def notifyVertexListeners(v: VertexChangedEvent[N]): Unit = for (callback <- vertexCallbacks.values) callback(v)
  }

  trait DefaultHandles[N, E <: SimpleEdgeLike[N]] extends ObservableGraph[N, E] {
    type EdgeHandle = ((EdgeChangedEvent[N, E]) => Unit)
    type VertexHandle = ((VertexChangedEvent[N]) => Unit)
    protected def createEdgeHandle(callback: (EdgeChangedEvent[N, E]) => Unit): EdgeHandle = callback
    protected def createVertexHandle(callback: (VertexChangedEvent[N]) => Unit): VertexHandle = callback
  }

  class ObservableSlimGraphUndirected[N <% Ordered[N], E <: SimpleEdgeLike[N]] extends SlimGraphUnDirected[N, E] with ObservableGraph[N, E] with DefaultHandles[N, E] {

    override def insertEdge(e: E) = {
      val r = super.insertEdge(e)
      if (r) { notifyEdgeListeners(EdgeAddedEvent(e)) }
      r
    }
    override def removeEdge(e: E) = {
      val r = super.removeEdge(e)
      if (r) { notifyEdgeListeners(EdgeRemovedEvent(e)) }
      r
    }
    override def insertVertex(n: N) = {
      val r = super.insertVertex(n)
      if (r) { notifyVertexListeners(VertexAddedEvent(n)) }
      r
    }
    override def removeVertex(n: N) = {
      val r = super.removeVertex(n)
      if (r) { notifyVertexListeners(VertexRemovedEvent(n)) }
      r
    }
  }

  class NeighborCache[N, E <: SimpleEdgeLike[N], G <: ObservableSlimGraphUndirected[N, E]](graph: G) {
    private val VertexHandle = graph.observeVertex(modifiedVertex)
    private val EdgeHandle = graph.observeEdge(modifiedEdge)

    //case class MarkedNeighbors( var changed: Boolean = true, val neighbors: Set[N] = Set.empty[N] )
    val neighbors = Map.empty[N, Set[N]]

    def stopObserving {
      graph.unobserveEdge(EdgeHandle)
      graph.unobserveVertex(VertexHandle)
    }

    def modifiedVertex(ve: VertexChangedEvent[N]) {
      ve match {
        case VertexAddedEvent(v) => Unit
        case VertexRemovedEvent(v) => {
          if (neighbors.contains(v)) {
            neighbors(v) = Set.empty[N]
          }
        }

      }
    }

    def modifiedEdge(ee: EdgeChangedEvent[N, E]) {
      ee match {
        case EdgeAddedEvent(e) => {
          if (neighbors.contains(e.source)) { neighbors(e.source) += e.target }
          if (neighbors.contains(e.target)) { neighbors(e.target) += e.source }
        }

        case EdgeRemovedEvent(e) => {
          if (neighbors.contains(e.source)) { neighbors(e.source) -= e.target }
          if (neighbors.contains(e.target)) { neighbors(e.target) -= e.source }
        }

      }
    }

    def neighborVertexSet(v: N): Set[N] = {
      if (!neighbors.contains(v)) {
        neighbors += (v -> (Set.empty[N] ++ graph.neighborVertices(v)))
      }
      neighbors(v)
    }

  }

  object InOutEdges {
    def empty[N, E <: SimpleEdgeLike[N]]() = {
      InOutEdges(Map.empty[N, E], Map.empty[N, E])
    }
  }
  
  case class InOutEdges[N, E <: SimpleEdgeLike[N]](incoming: Map[N, E], outgoing: Map[N, E])

  trait SimpleSlimGraphLike[NodeT, EdgeT <: SimpleEdgeLike[NodeT]] {
    type N = NodeT
    type E = EdgeT
    val adjList = Map.empty[N, InOutEdges[N,E]]
    val edgeSet = Set.empty[E]

    def degree(v: N) = { adjList(v).incoming.size + adjList(v).outgoing.size }

    def empty: SimpleSlimGraphLike[NodeT,EdgeT]

    def size() = edgeSet.size

    def vertices = adjList.keys

    def hasEdge(e: E) = { edgeSet.contains(e) }

    def order() = adjList.size

    def edges = { edgeSet }

    def isDirected(): Boolean

    def insertEdge(e: E) = {
      if (!edgeSet.contains(e)) {
        edgeSet += e
        adjList.getOrElseUpdate(e.source, InOutEdges.empty[N,E]).outgoing += (e.target -> e)
        adjList.getOrElseUpdate(e.target, InOutEdges.empty[N,E]).incoming += (e.source -> e)
        true
      } else {
        false
      }
    }

    def removeEdge(u: N, v: N): Boolean = {
      val eo = edgeOption(u,v)
      if ( eo.isDefined ) { removeEdge( eo.get ) } else { false }
    }

    def removeEdge(e: E) = {
      if (edgeSet.contains(e)) {
        edgeSet -= e
        adjList(e.source).outgoing -= e.target
        adjList(e.target).incoming -= e.source
        true
      } else {
        false
      }
    }

    def insertVertex(u: N) = {
      if (!adjList.contains(u)) {
        adjList(u) = InOutEdges.empty[N,E]
        true
      } else {
        false
      }
    }

    def removeVertex(u: N) = {
      if (adjList.contains(u)) {
        adjList(u).outgoing.foreach {
          case (v, e) =>
            adjList(v).incoming -= u
            edges -= e
        }
        adjList(u).incoming.foreach {
          case (v, e) =>
            adjList(v).outgoing -= u
            edges -= e
        }
        adjList -= u
        true
      } else {
        false
      }
    }

    def neighborEdges(u: N) = { (adjList(u).incoming.valuesIterator ++ adjList(u).outgoing.valuesIterator) }

    def neighborVertices(u: N) = { (adjList(u).incoming.keysIterator ++ adjList(u).outgoing.keysIterator) }

    def neighbors(u: N) = { (adjList(u).incoming.iterator ++ adjList(u).outgoing.iterator) }

    def hasEdge(u: N, v: N): Boolean

    def hasVertex(u: N): Boolean = { adjList.contains(u) }

    def edgeOption(u: N, v: N): Option[E]

  }

  class SlimGraphUnDirected[NodeT <% Ordered[NodeT], EdgeT <: SimpleEdgeLike[NodeT]]() extends SimpleSlimGraphLike[NodeT, EdgeT] {
   override type N = NodeT
   override type E = EdgeT
   def empty: SlimGraphUnDirected[N,E] = {
      new SlimGraphUnDirected[N,E]()
    }
    def isDirected() = { false }

    def copy = {
	var cloner = new com.rits.cloning.Cloner();
	var clone = cloner.deepClone(this)
	clone
    }

    def hasEdge(u: N, v: N) = {
      if (adjList.contains(u)) {
        if (u < v) { adjList(u).outgoing.contains(v) } else { adjList(u).incoming.contains(v) }
      } else { false }
    }

    def edgeOption(u: N, v: N): Option[E] = {
      if (adjList.contains(u)) {
        if (u < v) {
          adjList(u).outgoing.lift(v)
        } else {
          adjList(u).incoming.lift(v)
        }
      } else {
        Option.empty[E]
      }
    }

    /*
  def connectedComponents = {
    val components = ArrayBuffer[SlimGraph[N]]()
    val seen = Set[N]()
    
    nodes.foreach{ v=>
      
        if (!seen.contains(v)) {
          val subcomp = Set[N]()
          
          traverse(v){ x=> 
              subcomp += x
              seen += x
            true
          }
          
          components += subgraph(subcomp)
          
        }
    }
    components
  }
  */
    /*
  def subgraph(nodeSubset: Set[N]) = {
    val subg = SlimGraph[N, E]()

    nodeSubset.foreach { u=>
      adjList(u).foreach{ v =>
        if (nodeSubset.contains(v)) { subg.insertEdge(u,v) }
      }
    }

   subg
  }
  */

    /*
  def traverse(t: N)(f: N => Boolean) {
    var nextLevel = Set.empty[N]
    val visited = Set.empty[N]
    nextLevel += t

    while ( nextLevel.size > 0 ) {
      val thisLevel = nextLevel
      nextLevel = Set.empty[N]

      thisLevel.foreach{ u =>

        if ( !visited(u) ) {
          visited.add( u )
          if(f(u)) {neighbors(u).foreach{ uval => nextLevel.add(uval)}}
        }

      }

    }
  }
  */
    /**
     * def completesTriangle(v: N, w: N): Boolean = {
     * adjList.foreach{ case(u, s) =>
     * if (s.contains(v) && s.contains(w)) { return true }
     * }
     * return false
     * }
     */

  }



  class SlimGraphDirected[NodeT <% Ordered[NodeT], EdgeT <: SimpleEdgeLike[NodeT]]() extends SimpleSlimGraphLike[NodeT, EdgeT] {
    override type N = NodeT
    override type E = EdgeT
    def empty: SlimGraphDirected[N,E] = {
      new SlimGraphDirected[N,E]()
    }

    def isDirected() = { true }

    def hasEdge(u: N, v: N) = { adjList.contains(u) && adjList(u).outgoing.contains(v) }

    def edgeOption(u: N, v: N): Option[E] = {
      if (adjList.contains(u) && adjList(u).outgoing.contains(v)) {
        Some(adjList(u).outgoing(v))
      } else {
        Option.empty[E]
      }
    }

    def copy = {
      val g = new SlimGraphDirected[N, E]()
      vertices.foreach { u => g.insertVertex(u) }
      edges.foreach { e => g.insertEdge(e) }
      g
    }

    def incomingEdges(u: N) = { adjList(u).incoming.valuesIterator }
    def outgoingEdges(u: N) = { adjList(u).outgoing.valuesIterator }
    def predecessors(u: N) = { adjList(u).incoming.keysIterator }
    def successors(u: N) = { adjList(u).outgoing.keysIterator }

    /**
     * Return an array of the vertices in a topologically sorted order;
     * raises an exception if the graph is cyclic or undirected
     *
     * DFS based on the wikipedia article at http://en.wikipedia.org/wiki/Topological_sorting
     */
    def topologicalOrdering(): ArrayBuffer[N] = {
      val L = new ArrayBuffer[N](order)
      val visited = HashSet.empty[N]

      // All nodes with no outgoing edges
      val S = vertices.filter { n => successors(n).isEmpty }
      S.foreach { n => visit(n) }

      def visit(u: N) {
        if (!visited.contains(u)) {
          visited += u
          predecessors(u).foreach { v => visit(v) }
          L += u
        }
      }
      // We have to have everything in L
      assert(L.size == vertices.size, "Size of topo order list is %d, but there are %d nodes".format(L.size, vertices.size))
      L
    }

  }

class UnionFind[X]{
  
  case class PR( var parent: X, var rank: Int )

  private val prmap = Map.empty[X, PR]

  def addNode( x: X ) = { prmap(x) = PR(x, 0) }

  def union(x : X, y: X): Unit = {
    val xRoot = prmap(find(x))
    val yRoot = prmap(find(y))
    if (xRoot == yRoot) { return }

    if (xRoot.rank < yRoot.rank) {
      xRoot.parent = yRoot.parent
    } else if (xRoot.rank > yRoot.rank) {
      yRoot.parent = xRoot.parent
    } else {
      yRoot.parent = xRoot.parent
      xRoot.rank = xRoot.rank + 1
    }
  }

  def find( xi: X ): X = {
    val x = prmap(xi)
    if ( x.parent != xi ) {
      x.parent = find(x.parent)
    }
    return x.parent
  }

}



  object EdgeGraphAlgorithms {
    import scala.collection.mutable.PriorityQueue

    /** WHY DO WE NEED THE REDUNDANT TYPE BOUNDS ( E <: SimpleEdgeLike[N] )?
    */
    def minimumSpanningTree[
      T,
      N, 
      E <: SimpleEdgeLike[N],
      G <: SimpleSlimGraphLike[N,E]](
      graph: G with SimpleSlimGraphLike[N,E],
      wf:(E) => T )(implicit mf: Manifest[G], ev1: T => Ordered[T], ev2: N => Ordered[N]) = {
      


      val ctor = mf.erasure.getConstructor(classOf[N => Ordered[N]])
      val graphc = ctor.newInstance(ev2).asInstanceOf[G]

      val ufinder = new UnionFind[N]()
      
      graph.vertices.foreach{ n => ufinder.addNode(n) }

      val edgeQueue = PriorityQueue.empty( 
        Ordering.fromLessThan[E]( (a,b) => {
          val w1:T = wf(a)
          val w2:T = wf(b)
          w1 >= w2 
          }))

      edgeQueue ++= graph.edges.iterator

      val graphOrder = graph.order
      var totalSpanned = 0
      while ( edgeQueue.nonEmpty && totalSpanned < graphOrder ) {
        val e = edgeQueue.dequeue
        if ( ufinder.find(e.source) != ufinder.find(e.target) ) {
          ufinder.union(e.source, e.target)
          graphc.insertEdge(e)
          totalSpanned += 1
        }
      }

      graphc
    }


    def triangles[N, E <: SimpleEdgeLike[N]](
      graph: SlimGraphUnDirected[N, E],
      f: ((N, N, N)) => Boolean = (t: (N, N, N)) => true) = {
      val tris = ArrayBuffer[(N, N, N)]()
      val graphc = graph.copy
      def otherVertex(u: N, e: E) = {
        if (e.source == u) { e.target } else { e.source }
      }

      graphc.vertices.foreach { v =>
        val vn = graphc.neighborEdges(v)

        vn.foreach { e =>
          graphc.removeEdge(e)
          val w = otherVertex(v, e)
          val wn = graphc.neighborVertices(w).toSet
          val vnew = graphc.neighborVertices(v).toSet
          (vnew & wn).foreach { x =>
            val t = ((v, w, x))
            if (f(t)) { tris += ((v, w, x)) }
          }

        }

      }
      tris
    }

    def trianglesByEdge[N, E <: SimpleEdgeLike[N]](
      graph: SlimGraphUnDirected[N, E],
      f: ((E, E, E)) => Boolean = (t: (E, E, E)) => true,
      verbose: Boolean = false ) = {

      val tris = ArrayBuffer[(E, E, E)]()
      if ( verbose ) { println("copying the graph") }
      val graphc = graph.copy
      if( verbose ) { println("done") }

      def otherVertex(u: N, e: E) = {
        if (e.source == u) { e.target } else { e.source }
      }

      var pnum = 0L
      var numTris = 0L

      while (graphc.edges.size > 0) {
        val e1 = graphc.edges.head
        graphc.removeEdge(e1)
        val u = e1.source
        val v = e1.target
        var neigh = graphc.neighbors(u) //neighborEdges(u)
        neigh.foreach {
          case (w, e2) =>
            val e3 = graphc.edgeOption(v, w)
            if (e3.isDefined) {
              numTris += 1
              val t = ((e1, e2, e3.get))
              if (f(t)) { tris += (t) }
            }
        }
        if( verbose && numTris - pnum > 10000) { 
          pnum = numTris
          print("\r")
          print("\r\r"+numTris+" total and "+tris.size+" valid triangles ")                  
        }
      }
      println()
      tris
    }

    /*
    def trianglesByEdgeParallel[N <% Ordered[N], E <: SimpleEdgeLike[N]](
      graph: SlimGraphUnDirected[N,E],
      f: ((E, E, E)) => Boolean = (t: (E, E, E)) => true,
      verbose: Boolean = false ) = {

      import scala.collection.mutable._
      import akka.dispatch.{ Await, ExecutionContext, Promise, Future }
      import akka.actor.{ ActorSystem }
      import com.typesafe.config.ConfigFactory
      import akka.util.Duration
      import java.util.concurrent.TimeUnit

      var lock: AnyRef = new Object()
      //implicit val ec = ExecutionContext.fromExecutorService(yourExecutorServiceGoesHere)
      var globalTris = new Array[Array[(E,E,E)]]( graph.edges.size )
      
      val confFile = new java.io.File("config/mmsfilter.conf")
      val customConf = ConfigFactory.parseFile( confFile )
      implicit val system = ActorSystem("TrianlgeActors", customConf)

      var numTris = 0L

      def trianglesForEdge( e: E, en: Int ) = {
        var vt = 0
        val u = e.source
        val v = e.target
        graph.neighbors(v).foreach{ case(w,e2) =>
          if ( w > v ) {
            val e3 = graph.edgeOption(v, w)
            if (e3.isDefined) {
              val t = ((e, e2, e3.get))
              if (f(t)) { vt += 1 }
            }
          }
        }
        var tris = new Array[(E,E,E)](vt)
        vt = 0
        graph.neighbors(v).foreach{ case(w,e2) =>
          if ( w > v ) {
            val e3 = graph.edgeOption(v, w)
            if (e3.isDefined) {
              numTris += 1
              if (verbose && numTris % 10000 == 0 ){
                print("\r")
                print("\r\r Counted "+numTris+" triangles ")
              }
              val t = ((e, e2, e3.get))
              if (f(t)) { tris(vt) = t; vt += 1 }
            }
          }
        }
        (tris,en)
      }
      
      var numEdgesCounted = 0L
      var en = 0
      graph.edges.zipWithIndex.view.foreach{ case (e,i) =>
        Future{ trianglesForEdge(e,i) }.onSuccess{ 
          case (x,n) => {
            globalTris(n) = x
            numEdgesCounted += 1
          }
        } 
      }

      while( numEdgesCounted < graph.edges.size ) {
        Thread.sleep(1000)
      }
  
      //val futureList = Future.traverse( graph.edges )( e => Future{ trianglesForEdge(e) } )
      //val futureList = Future.fold( futures )( ArrayBuffer.empty[(E,E,E)] )( _ ++ _ )
      //Await.result(futureList, Duration.Inf)
      globalTris
      /*
      def otherVertex(u: N, e: E) = {
        if (e.source == u) { e.target } else { e.source }
      }


      while (graphc.edges.size > 0) {
        val e1 = graphc.edges.head
        graphc.removeEdge(e1)
        val u = e1.source
        val v = e1.target
        var neigh = graphc.neighbors(u) //neighborEdges(u)
        neigh.foreach {
          case (w, e2) =>
            val e3 = graphc.edgeOption(v, w)
            if (e3.isDefined) {
              numTris += 1
              val t = ((e1, e2, e3.get))
              if (f(t)) { tris += (t) }
            }
        }
        if( verbose && numTris - pnum > 10000) { 
          pnum = numTris
          print("\r")
          print("\r\r"+numTris+" total and "+tris.size+" valid triangles ")                  
        }
      }
      println()
      tris
      */
    }
    */

    def fastTriangles[N <% Ordered[N], E <: SimpleEdgeLike[N]](
      graph: SlimGraphUnDirected[N, E],
      f: ((N, N, N)) => Boolean = (t: (N, N, N)) => true) = {
      val tris = ArrayBuffer[(N, N, N)]()
      val graphc = new ObservableSlimGraphUndirected[N, E]()
      graph.edges.foreach { e => graphc.insertEdge(e) }
      graph.vertices.foreach { n => graphc.insertVertex(n) }
      val nc = new NeighborCache[N, E, graphc.type](graphc)

      def otherVertex(u: N, e: E) = {
        if (e.source == u) { e.target } else { e.source }
      }

      graphc.vertices.foreach { v =>
        val vn = graphc.neighborEdges(v)
        vn.foreach { e =>
          graphc.removeEdge(e)
          val w = otherVertex(v, e)
          val wn = nc.neighborVertexSet(w)
          val vnew = nc.neighborVertexSet(v)

          (vnew & wn).foreach { x =>
            val t = ((v, w, x))
            if (f(t)) { tris += ((v, w, x)) }
          }

        }

      }
      tris
    }
    /*
  def undirect[N <% Ordered[N]](digraph: SlimGraph[N]) = {
    val graph = SlimGraph[N]()
    digraph.edges.foreach{ case(v,w) => graph.insertEdge(v,w,false) }
    graph
  }
  */
  }

  object MMSEdge {
    def fromLine(s: String) = {
      val toks = s.split("""\s+""")
      new MMSEdge(toks(0), toks(1), toks(2).toDouble, toks(3).toDouble, toks(4).toDouble)
    }
  }
  /*
  case class UndirectedEdge[N <% Ordered[N]](u: N, v: N) extends UnorderedEdgeLike[N] {
    val (source, target) = if (u > v) { (v, u) } else { (u, v) }
  }
  */
  case class MMSEdge(u: String, v: String, val weight: Double, val freq: Double, val q: Double) extends UnorderedEdgeLike[String] {
    val (source, target) = if (u < v) { (u, v) } else { (v, u) }
  }

  object MMSGraphReader {

    def fromFile(fname: String) = {
      val fsrc = Source.fromFile(fname)
      val graph = new SlimGraphUnDirected[String, MMSEdge]()

      fsrc.getLines.foreach { l =>
        graph.insertEdge(MMSEdge.fromLine(l))
      }
      graph
    }

  }
}


