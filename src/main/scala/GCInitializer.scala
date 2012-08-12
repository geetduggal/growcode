package GrowCode
import ec._
import ec.simple._
import ec.vector._
import ec.util.{ Parameter => ECParam }
import scalax.collection.mutable.{ Graph => MGraph }
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scala.io.Source
import SlimGraph._
import SlimGraphAlgorithms._
import scalala.tensor.dense._

object EdgeListReader {

  def fromFile( fname: String ) = {
    val fsrc = Source.fromFile( fname )
    val graph = MGraph[Int, UnDiEdge]()

    fsrc.getLines.foreach{
        l =>
        if ( !l.trim.startsWith("#") ) { // ignore comments
          val toks = l.split("""\s+""").toList.map{ v => v.toInt }
          graph += toks(0)~toks(1)
        }
     }

    fsrc.close()
    graph
  }


  def fromFileSG( fname: String ) = {

    val fsrc = Source.fromFile( fname )
    val graph = new SlimGraphUnDirected[Int, UndirectedEdge[Int]]()

    fsrc.getLines.foreach{
        l =>
        if ( !l.trim.startsWith("#") ) { // ignore comments
          val toks = l.split("""\s+""").toList.map{ v => v.toInt }
          graph.insertEdge( UndirectedEdge(toks(0),toks(1)) )
        }
     }

    fsrc.close()
    graph
  }

}

class GCInitializer extends SimpleInitializer {
  var targetGraph = new SlimGraphUnDirected[Int, UndirectedEdge[Int]]()
  var targetShape = DenseVector.zeros[Double](0)
  var targetDiameter = 0
  var targetAssortativity = 0.0
  var targetClustering = 0.0
  var targetAvgClustering = 0.0
  var targetEigCentrality = Array.empty[Double]

  override def initialPopulation( state: EvolutionState, thread: Int ) = {
    // read in target graph
    val graphFileName = state.parameters.getString( new ECParam("targetGraph"), null )
    //targetGraph = GraphReader.EdgeListReader.fromFile(graphFileName, (a:Int, b:Int) => UndirectedEdge(a,b) )
    targetGraph = EdgeListReader.fromFileSG( graphFileName )
    println("|V|="+targetGraph.order+" |E|="+targetGraph.size)
    targetShape = shape(targetGraph)
    targetDiameter = 10//diameter(targetGraph)
    targetAssortativity = assortativity(targetGraph)
    targetAvgClustering = avgClustering(targetGraph)
    //targetClustering = clustering(targetGraph)
    println("AVG Clustering\t"+avgClustering(targetGraph))
    targetEigCentrality = eigenvectorCentrality(targetGraph)

    super.initialPopulation(state, thread)
  }

}
