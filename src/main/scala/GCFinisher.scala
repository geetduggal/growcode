package GrowCode
import ec._
import ec.simple._
import ec.vector._
import ec.util.{ Parameter => ECParam }
import scala.collection.mutable.ArrayBuffer
import scalax.collection.mutable.{ Graph => MGraph }
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.io._
import scalax.file.Path
import scalax.io.JavaConverters._
import scala.collection.JavaConversions._
import ec.multiobjective._

class GCFinisher extends SimpleFinisher {

  override def finishPopulation( state: EvolutionState, thread: Int ) {

    var bestInd : GeneVectorIndividual = null
    var bestFitness = Float.MinValue

    var individuals = ArrayBuffer.empty[ Program ]

    // Iterate over all subpopulations and all species in each subpop
    // extract the most fit individual, as well as their fitness score
    state.population.subpops.foreach{ subpop =>
      subpop.individuals.foreach{ individual =>
        val ind = individual.asInstanceOf[GeneVectorIndividual]
        val prog = new Program( ind.genome.map{ gcb => gcb.asInstanceOf[GCBit].op }.toArray )
        prog.properties("fitness") = ind.fitness.asInstanceOf[MultiObjectiveFitness].getObjectives.mkString(",")
        individuals += prog
      }
   }

    /*
      // get the most fit inidividual in this subpop
      val ind = subpop.individuals.minBy{  ind => ind.fitness.fitness() }
      // if it's the most fit overall, store it
      if ( ind.fitness.fitness() >= bestFitness ) {
        bestInd = ind.asInstanceOf[GeneVectorIndividual]
        bestFitness = ind.fitness.fitness()
      }
    }
    */

    // We're going to write out the program corresponding to the most fit individual
    // to the file whose name is stored in "outputFile" in the parameter database
    val outputFileName = state.parameters.getString( new ECParam("outputFile"), null )

    // Create a resource from the filename
    Marshall.writeToFile( outputFileName, individuals )
    /*
    Path(outputFileName).createFile()
    val output : Output = Resource.fromFile(outputFileName)

    // Write the individual to the file
    val ostr = bestInd.asInstanceOf[GeneVectorIndividual].genome.map{ gene =>
        gene.asInstanceOf[GCBit].op.Str
    }.mkString("\n")

    output.write(ostr)
    */

    // Call our parent's finishPopulation method
    super.finishPopulation(state, thread)
    // Done!
  }

}
