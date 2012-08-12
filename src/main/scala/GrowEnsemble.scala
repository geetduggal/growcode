
package GrowCode

import GrowCode.GrowthModels._
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scalala.tensor.dense._
import scala.actors.Future
import scala.actors.Futures._
import scalax.io._
import scalax.file._
import scala.io.Source
import scala.sys.process._

// For command line parsing
import org.rogach.scallop._

import GrowCode.GraphAlgorithms._

// GrowCode is Awesome

  object GrowEnsemble {

      def main( argv:Array[String] ) = {

       /**
       * Configuration Options
       */
      object Conf extends ScallopConf(argv) {

        version("1.0")
        banner("""Usage: GrowEnsemble [OPTION]
                  |
                  | GrowEnsemble lets you grow an ensemble of graphs
                  | from a given GrowCode program.
                  |
                  |Options:
                  |""".stripMargin)

        val inputFileName = opt[String]("input", 
          descr = "file containing the input program", required = true )

        val numGraphs = opt[Int]("numGraphs",
          descr = "The size of the ensemble to generate", required = true)
        
        val numIt = opt[Int]("iterations",
          descr = "The number of iterations to grow the graphs", required = true)

        val outDir = opt[String]("outDir", 
          descr = "directory where the output will be written", required = true )

        val numThreads = opt[Int]("threads",
          descr = "number of threads to run in parallel", required = false, default = Some(2))

        try {
          verify
        } catch {
          case v: exceptions.ValidationFailure => println("Error parsing arguments; exiting"); sys.exit()
        }
      }


        val inputProg = Conf.inputFileName()
        val numGraph = Conf.numGraphs()
        val numIt = Conf.numIt()
        val outDir = Path(Conf.outDir())
        var numThread = Conf.numThreads()
      
        // Resolve issue with Java7 and Scala 2.9.1 (SI-4960)
        val version = System.getProperty("java.runtime.version")
        if ( version.slice(0,3) == "1.7" ) { 
          System.setProperty("java.vm.vendor", "Sun Microsystems Inc.")
        }
      
        collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(numThread)

        outDir.createDirectory(true, false)

        val prog = Program.fromFile(inputProg)
        println("using program "+prog.shortStr)

        println("Generating an ensemble of "+numGraph+" random graphs")
        (0 until numGraph).par.foreach{ i =>
          println("graph "+i)
          val machine = new Machine
          machine.grow(prog, numIt, verbose=true)
          //val graph = DMC(2647, 0.54444, 0.3667)
          val ofile = Path(outDir.path+"/"+i+".edg")
          ofile.createFile(true,false)
          ofile.openOutput{ out =>
              machine.graph.edges.foreach{ e =>
              //graph.edges.foreach{ e =>
                out.write(e.source+"\t"+e.target+"\n")
              }
          }
        }

      }

}
