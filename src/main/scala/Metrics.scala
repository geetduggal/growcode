package SlimGraph

object Metrics {

	import scala.collection.mutable.IndexedSeqView

	/**
	* TODO : Specialized generization of this method for other array types?
	*/
	def corrcoef( a: IndexedSeqView[Double, Array[Double]], b: IndexedSeqView[Double, Array[Double]] ) = {
		val N = a.size
		val s = 1.0 / N
		var i = 0
		var (ea, eb) = (0.0, 0.0)
		while ( i < N ) {
			ea += s * a(i)
			eb += s * b(i)
			i += 1
		}

		i = 0
		var rn = 0.0
		var (siga, sigb) = (0.0, 0.0)
		while ( i < N ) {
			val ai = (a(i) - ea)
			val bi = (b(i) - eb)
			rn += ai * bi
			siga += ( ai*ai )
			sigb += ( bi*bi )
			i += 1
		}

		rn / (math.sqrt(siga)*math.sqrt(sigb))
	}

	def assortativity[N, E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
	graph: G with SimpleSlimGraphLike[N,E]): Double = {

		val degreeSource = Array.fill(2*graph.size+1)(0.0)
		val degreeTarget = Array.fill(2*graph.size+1)(0.0)
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
		
		val cc = corrcoef(degreeSource.view(0,i), degreeTarget.view(0,i))
		if ( cc.isNaN ) { 0.0 } else { cc }
	}

	/** Compute the clustering coefficient of node 'u' in graph 'graph'
	* @param graph The hosting graph
	* @param u The target node
	* @reuturns The clustering coefficient of u
	*/
	def clustering[N <% Ordered[N], E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
	graph: G with SimpleSlimGraphLike[N, E],
	u: N): Double = {

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

  /** Compute the average clustering coefficient of the graph 'graph'
  * @param graph The hosting graph
  * @returns The average clustering coefficient of 'graph'
  */
  def avgClustering[N <% Ordered[N], E <: SimpleEdgeLike[N], G <: SimpleSlimGraphLike[N,E]](
    graph: G with SimpleSlimGraphLike[N, E]): Double = {

    // Sum the per-vertex clustering coefficients and take the average
    var nvert = 0
    graph.vertices.toSeq.map{ u =>
      nvert += 1
      clustering(graph, u)
    }.sum / nvert

  }



}