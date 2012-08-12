package GrowCode
import scala.collection.JavaConversions._
import ec._
import ec.multiobjective._
import ec.simple._
import ec.vector._
import scala.math._
import ec.util.{ Parameter => ECParam }
import scalax.collection.mutable.{ Graph => MGraph }
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import SlimGraphAlgorithms._
import SlimGraph._
import java.io.FileWriter
import scala.actors.Future
import scala.actors.Futures._

class BestProgramProblem extends Problem with SimpleProblemForm
  {

  def evaluate(state: EvolutionState,
               ind: Individual,
               subpopulation: Int,
               threadnum: Int)
  {
    def ech(eigCentrality: Array[Double]) = {
      new Histogram(eigCentrality, Some((0.0,1.0)), 20, true).hist
    }

    if (ind.evaluated) return

    if (!(ind.isInstanceOf[GeneVectorIndividual]))
      state.output.fatal("Whoa!  It's not a GeneVectorIndividual!!!",null)

    var sum = 0
    val ind2: GeneVectorIndividual = ind.asInstanceOf[GeneVectorIndividual]

    val prog = new Program( ind2.genome.map{ x => x.asInstanceOf[GCBit].op } )

    val numIter = state.parameters.getInt( new ECParam("numGrowIter"), null )
    val graphTarget = state.initializer.asInstanceOf[GCInitializer].targetGraph

    print("*")

    val diameterTarget = state.initializer.asInstanceOf[GCInitializer].targetDiameter
    val shapeTarget = state.initializer.asInstanceOf[GCInitializer].targetShape
    val assortativityTarget = state.initializer.asInstanceOf[GCInitializer].targetAssortativity
    val clusteringTarget = state.initializer.asInstanceOf[GCInitializer].targetClustering
    val avgClusteringTarget = state.initializer.asInstanceOf[GCInitializer].targetAvgClustering
    val eigCentralityTarget = state.initializer.asInstanceOf[GCInitializer].targetEigCentrality

    /*
    var editDists = 0.0
    var nInds = 0
    state.population.subpops.foreach{ subpop =>
      subpop.individuals.foreach{ individual =>
        val ind = individual.asInstanceOf[GeneVectorIndividual]
        val progi = new Program( ind.genome.map{ gcb => gcb.asInstanceOf[GCBit].op }.toArray )
        val ga = new GlobalAlignment(prog.shortStr, progi.shortStr)
        ga.align()
        val score = ga.score + min(prog.length, progi.length)
        editDists += score/(prog.length*progi.length)
        nInds += 1
      }
      editDists /= subpop.individuals.size
    }

    // Have normalized edit distance scores be approximately on the same footing as distribution matching scores
    editDists *= .1
    */

    var fitShape = List.empty[Float]
    var fitLength = 0.f
    var fitDiameter = 0.f
    var fitAssortativity = List.empty[Float]
    var fitEigCentrality = 0.f
    var fitClustering = 0.f
    var fitAvgClustering = List.empty[Float]

    (0 until 5).foreach{ i =>
      val machine = new Machine
      val f = future{ machine.grow(prog, numIter) }
      val yay = GrowGraph.runOrDie(120000, f, () => { println("We gone done murda:"+prog.shortStr); machine.halt = true } )
      //var calculate = !yay.isDefined
      var calculate = true

      var fitOrder = abs(graphTarget.order - machine.graph.order)
      var fitSize = abs(graphTarget.size - machine.graph.size)

      if (fitOrder < graphTarget.order/2.0 && fitSize < graphTarget.size/2.0 && calculate) {

        //val diameterIndividual = diameter(machine.graph).toFloat
        val shapeThis = shape(machine.graph)
        val diff = (shapeTarget - shapeThis).norm(1.0)
        //val eigCentralityIndividual = eigenvectorCentrality(machine.graph)

        fitShape = (1/abs(-diff-.0001)).toFloat :: fitShape
        //fitLength = 1/((prog.length+1).toFloat)
        //if (prog.length <= 5) { fitLength = 0 }
        //fitDiameter = 1.0f-abs(diameterTarget-diameterIndividual)/(diameterTarget+diameterIndividual)
        fitAssortativity = 1-abs(assortativity(machine.graph)+1 -(assortativityTarget+1)).toFloat/2.f :: fitAssortativity
        //fitClustering = 1-abs(clustering(machine.graph)-(clusteringTarget)).toFloat
        fitAvgClustering = 1-abs(avgClustering(machine.graph)-(avgClusteringTarget)).toFloat :: fitAvgClustering
        //val diffEig = (ech(eigCentralityIndividual)-ech(eigCentralityTarget)).norm(2).toFloat
        //fitEigCentrality  = (1/abs(-diffEig-.0001)).toFloat
      } else {
        fitShape = 0.f :: fitShape
        fitAvgClustering = 0.f :: fitAvgClustering
        fitAssortativity = 0.f :: fitAssortativity
      }
    }

    val avgFitAvgClustering = fitAvgClustering.sum / fitAvgClustering.size
    val avgFitAssortativity = fitAssortativity.sum / fitAssortativity.size
    val avgFitShape = fitShape.sum / fitShape.size

    //ind2.fitness.asInstanceOf[MultiObjectiveFitness].setObjectives( state, Array(fitShape, fitLength, fitDiameter, fitAssortativity) )
    
    /** Assortativity and shape */
    //ind2.fitness.asInstanceOf[MultiObjectiveFitness].setObjectives( state, Array(avgFitAssortativity, avgFitShape) )
    
    /** Assortativity, Shape and Avg. Clustering Coeff. */
    ind2.fitness.asInstanceOf[MultiObjectiveFitness].setObjectives( state, Array(avgFitAssortativity, avgFitShape, avgFitAvgClustering) )

  /*
    val degs = machine.graph.nodes.toSeq.map({v => v.neighbors.size.toDouble}).sortWith({(a,b) => a > b})
    val (scaleFree, alpha) = GrowGraph.analyzeDegreeDist(degs, 50000)
    //if (scaleFree) { println("Found scale free graph: alpha="+alpha+" fitness: "+fitShape) }
    val fw = new FileWriter("ScaleFreeEasy.txt", true) ;
    var sf = 0
    if (scaleFree) { sf = 1 }
    fw.write("%d\t%f\t%f\t%d\n".format(state.generation, alpha, fitShape, sf)) ;
    fw.close()
  */


    ind2.evaluated = true
  }
}
