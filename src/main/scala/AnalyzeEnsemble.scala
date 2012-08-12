package GrowCode
import org.clapper.argot.{ArgotParser, ArgotUsageException}
import scala.collection.mutable.ArrayBuffer
import net.robpatro.utils.console.ProgressBar

import scala.math._
import scalala.tensor.dense._
import scalala.library.Plotting._
import java.awt.Color

import scala.swing._
import scala.swing.Dialog.Result

import scala.actors.Future
import scala.actors.Futures._
import scalax.io._
import scalax.file.Path
import Path._
import scalax.file.PathSet
import scala.io.Source
import scala.sys.process._

import GrowCode.SlimGraphAlgorithms._

// GrowCode is Awesome
object AnalyzeEnsemble{

  // Class to hold the actual program options
  class ProgOptions {
    var ensembleDir = ""
    var outFile = ""
    var origGraph = ""
  }

  // test fast commit
  // another commit test

  // Perform the parsing and return a ProgOptions instance
  def makeParser( args: Array[String] ) = {
    import org.clapper.argot.ArgotConverters._
    // The argument parser
    val parser = new ArgotParser(
      "AnalyzeEnsemble",
      preUsage=Some("GrowGraph: Version 1.0 Copyright(c) "+
                    "2012, Geet Duggal & Rob Patro\n")
    )
    val printUsage = parser.flag[Boolean](List("h", "help"), "Print this help message.")
    var ensembleDir = parser.option[String](List("i","inputDir"), "i", "Directory containing input graphs" )
    var origGraph = parser.option[String](List("g", "origGraph"), "g", "Original (Target) graph")
    var outFile = parser.option[String](List("o","output"), "o", "Where output should be written")
    try {
      parser.parse(args)
    }  catch {

      case e: ArgotUsageException => {
        if (printUsage.value.isDefined) {
          println(parser.usageString())
          sys.exit(0)
        } else {
          println(e.message)
          sys.exit(0)

        }
      }

    }

    val po = new ProgOptions

    require( ensembleDir.value.isDefined, {println("name of input directory")})
    po.ensembleDir = ensembleDir.value.get

    require( outFile.value.isDefined, {println("name of output file")})
    po.outFile = outFile.value.get

    require( origGraph.value.isDefined, {println("name of original file")})
    po.origGraph = origGraph.value.get

    po
  }


  def main( args:Array[String] ) {

      // Parse the command line options
      var po = new ProgOptions
      po = makeParser(args)
      val inputPathSet:PathSet[Path] = po.ensembleDir ** "*.edg"
      val ofile:Path = Path(po.outFile)
      var pb = new ProgressBar(inputPathSet.size, "=")
      var gn = 0
      val tgtG = EdgeListReader.fromFileSG(po.origGraph)
      val tgtShape = shape(tgtG)
      val tgtAssort = assortativity(tgtG)
      val tgtAvgClust = avgClustering(tgtG)

      ofile.createFile(true,false)
      ofile.openOutput{ out =>
        out.write("TargetAssortativity=%f\tTargetAvgClustering=%f\tTargetShapeDist=0.0\n".format(tgtAssort, tgtAvgClust))
        inputPathSet.foreach{ ifile =>
            pb.update(gn)
            val G = EdgeListReader.fromFileSG(ifile.path)
            val assort = assortativity(G)
            val avgClust = avgClustering(G)
            val cShape = shape(G)
            val shapeDiff = (tgtShape - cShape).norm(1.0)

            out.write(assort+"\t"+avgClust+"\t"+shapeDiff+"\n")
            gn += 1
          }
      }
      pb.done()
  }

}
