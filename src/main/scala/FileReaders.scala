import scala.io.Source

package SlimGraph {
  package object GraphReader {
    implicit val asInt = (s: String) => { augmentString(s).toInt }
    implicit val asLong = (s: String) => { augmentString(s).toLong }
    implicit val asFloat = (s: String) => { augmentString(s).toFloat }
    implicit val asDouble = (s: String) => { augmentString(s).toDouble }
  }

  package GraphReader {

    trait DefaultReader {

      def _readFromFile[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N, E]](
        fname: String,
        graph: G,
        efact: (N, N) => E,
        nfact: (String) => N): Boolean

      def fromFile[N](
        fname: String,
        efact: (N, N) => UndirectedEdge[N])(implicit ev: N => Ordered[N], nfact: (String) => N ) = {
        val g = new SlimGraphUnDirected[N, UndirectedEdge[N]]()
        _readFromFile(fname, g, efact(_:N, _:N), nfact(_:String))
        g
      }

      def fromFile[N](
        fname: String,
        efact: (N, N) => DirectedEdge[N])(implicit ev: N => Ordered[N], nfact: (String) => N ) = {
        val g = new SlimGraphDirected[N, DirectedEdge[N]]()
        _readFromFile(fname, g, efact(_:N, _:N), nfact(_:String))
        //_readFromFile(fname, g, DirectedEdge.factory[])
        g
      }

      def fromFile[T, N](
        fname: String,
        weight: T,
        efact: (N, N, T) => WeightedUndirectedEdge[N,T])(implicit ev: N => Ordered[N], nfact: (String) => N ) = {
        val g = new SlimGraphUnDirected[N, WeightedUndirectedEdge[N, T]]()
        _readFromFile(fname, g, efact(_: N, _: N, weight), nfact(_:String))
        g
      }

      def fromFile[T, N](
        fname: String,
        weight: T,
        efact: (N, N, T) => WeightedDirectedEdge[N,T])(implicit ev: N => Ordered[N], nfact: (String) => N ) = {
        val g = new SlimGraphDirected[N, WeightedDirectedEdge[N, T]]()
        _readFromFile(fname, g, efact(_: N, _: N, weight), nfact(_:String))
        g
      }

    }

    trait WeightedReader extends DefaultReader {
      def _readFromFile[T, N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N, E]](
        fname: String,
        graph: G,
        efact: (N, N, T) => E,
        nfact: (String) => N)(implicit weightFun: (String) => T): Boolean

      def fromFile[T, N](
        fname: String,
        efact: (N, N, T) => WeightedDirectedEdge[N, T])(implicit weightFun: (String) => T, ev: N => Ordered[N], nfact: (String) => N) = {
        val g = new SlimGraphDirected[N, WeightedDirectedEdge[N, T]]()
        _readFromFile(fname, g, efact(_:N, _:N, _:T), nfact(_:String))(weightFun(_:String))//WeightedDirectedEdge.factory[N, T])
        g
      }

      def fromFile[T, N](
        fname: String,
        efact: (N, N, T) => WeightedUndirectedEdge[N, T])(implicit weightFun: (String) => T, ev: N => Ordered[N], nfact: (String) => N) = {
        val g = new SlimGraphUnDirected[N, WeightedUndirectedEdge[N, T]]()
        _readFromFile(fname, g, efact(_:N, _:N, _:T), nfact(_:String))(weightFun(_:String))//WeightedUndirectedEdge.factory[N, T])
        g
      }
    }

    object EdgeListReader extends DefaultReader with WeightedReader {
      def _readFromFile[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N, E]](
        fname: String,
        graph: G,
        efact: (N, N) => E,
        nfact: (String) => N ) = {

        val fsrc = Source.fromFile(fname)
        val directed = graph.isDirected

        fsrc.getLines.foreach { l =>
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks = l.split("""\s+""").toList
            val e = efact( nfact(toks(0)), nfact(toks(1)) ) 
            graph.insertEdge(e)
          }
        }
        true
      }

      def _readFromFile[T, N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N, E]](
        fname: String,
        graph: G,
        efact: (N, N, T) => E,
        nfact: (String) => N)(implicit weightFun: (String) => T) = {

        val fsrc = Source.fromFile(fname)
        val directed = graph.isDirected

        fsrc.getLines.foreach { l =>
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks = l.split("""\s+""").toList
            //val e = efact(toks(0), toks(1), toks(2))
            graph.insertEdge(efact(nfact(toks(0)), nfact(toks(1)), weightFun(toks(2))))
          }
        }
        true
      }

    }

    object AdjListReader extends DefaultReader {
      def _readFromFile[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N, E]](
        fname: String,
        graph: G,
        efact: (N, N) => E,
        nfact: (String) => N ) = {

        val fsrc = Source.fromFile(fname)

        fsrc.getLines.foreach { l =>
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks = l.split("""\s+""").toList
            val u = nfact(toks.head)
            val vList = toks.tail
            graph.insertVertex(u)
            vList.foreach { vs => val v = nfact(vs); graph.insertEdge(efact(u, v)) }
          }
        }
        true
      }

    }

    object MultiAdjListReader extends DefaultReader with WeightedReader {
      def _readFromFile[T, N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N, E]](
        fname: String,
        graph: G,
        efact: (N, N, T) => E,
        nfact: (String) => N)(implicit weightFun: (String) => T) = {

        val lineIt = Source.fromFile(fname).getLines

        def addNeighbors(u: N, n: Int) {
          (0 until n).foreach { i =>
            val l = lineIt.next
            val toks = l.split("""\s+""").toList
            val v = nfact(toks(0))
            //val weight = toks(1).toDouble 
            graph.insertEdge(efact(u, v, weightFun(toks(1))))
          }
        }

        while (lineIt.hasNext) {
          val l = lineIt.next
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks: List[String] = l.split("""\s+""").toList
            val (u, numVerts) = (nfact(toks(0)), asInt(toks(1)))
            graph.insertVertex(u)
            addNeighbors(u, numVerts)
          }
        }
        true
      }

      def _readFromFile[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N, E]](
        fname: String,
        graph: G,
        efact: (N, N) => E,
        nfact: (String) => N ) = {

        val lineIt = Source.fromFile(fname).getLines

        def addNeighbors(u: N, n: Int) {
          (0 until n).foreach { i =>
            val l = lineIt.next
            val toks = l.split("""\s+""").toList
            val v = nfact(toks(0))
            //val weight = toks(1).toDouble 
            graph.insertEdge(efact(u, v))
          }
        }

        while (lineIt.hasNext) {
          val l = lineIt.next
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks = l.split("""\s+""").toList
            val (u, numVerts) = (nfact(toks(0)), asInt(toks(1)))
            graph.insertVertex(u)
            addNeighbors(u, numVerts)
          }
        }
        true
      }

    }
  }
}