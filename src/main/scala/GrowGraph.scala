package GrowCode
import org.clapper.argot.{ArgotParser, ArgotUsageException}
import scala.collection.mutable.ArrayBuffer

import scala.math._
import scalala.tensor.dense._
import scalala.library.Plotting._
import java.awt.Color

import scala.swing._
import scala.swing.Dialog.Result

import scala.actors.Future
import scala.actors.Futures._
import scalax.io._
import scala.io.Source
import scala.sys.process._

import SlimGraph._
import GrowCode.SlimGraphAlgorithms._
import java.io.{File, FileWriter}

// GrowCode is Awesome

object GrowGraph {
    val logo =
      """
   ___                    ___           _
  / _ \_ __ _____      __/ __\ ___   __| | ___
 / /_\/ '__/ _ \ \ /\ / / /   / _ \ / _` |/ _ \
/ /_\\| | | (_) \ V  V / /___| (_) | (_| |  __/
\____/|_|  \___/ \_/\_/\____/ \___/ \__,_|\___|
  """

  // Class to hold the actual program options
  class ProgOptions {
    var numIt = 100
    var pTestTimeout: Long = 50000
    var progname = ""
    var progInd = 0
    var origGraph = ""
    var progOutFile = ""
  }

  // test fast commit
  // another commit test

  // Perform the parsing and return a ProgOptions instance
  def makeParser( args: Array[String] ) = {
    import org.clapper.argot.ArgotConverters._
    // The argument parser
    val parser = new ArgotParser(
      "GrowGraph",
      preUsage=Some("GrowGraph: Version 1.0 Copyright(c) "+
                    "2011, Geet Duggal & Rob Patro\n"+logo)
    )
    val printUsage = parser.flag[Boolean](List("h", "help"), "Print this help message.")
    var numIt = parser.option[Int](List("i", "iterations"), "n", "Number of interations to grow (default is 100)")
    var timeout = parser.option[Long](List("t", "timeout"), "t", "Time, in seconds, to wait for each p-value test (default is 50)")
    var progInd = parser.option[Int](List("p", "program"), "p", "Program index for scale free stats")
    var progOutFile = parser.option[String](List("o", "output"), "o", "Output file for selected GrowCode program")
    var origGraph = parser.option[String](List("g", "origGraph"), "g", "Original graph for printing statistics")
    var progname = parser.parameter[String]("growfile", "Grow file from which to read graph", false)
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
    po.numIt = numIt.value.getOrElse(100)
    require( progname.value.isDefined, {println("name of growcode file is a required parameter")})
    po.progname = progname.value.get
    po.progInd = progInd.value.getOrElse(-1)
    po.pTestTimeout = timeout.value.getOrElse(50L) * 1000
    po.origGraph = origGraph.value.getOrElse("")
    po.progOutFile = progOutFile.value.getOrElse("growgraph.grw")

    po
  }

  def runOrDie( t: Long, f: Future[Any], killfun: () => Unit ): Option[Any] = {
    val l = awaitAll(t, f)
    if ( ! l(0).isDefined ){
      killfun()
      return Option.empty[Any]
    }
    l(0)
  }

  def analyzeDegreeDist( degs:Seq[Double], timeout: Long ): (Boolean, Float) = {
    val dbytes = degs.map{ x => (x+1).toInt.toString }.mkString(" ").getBytes
    val obuf = new StringBuffer
    val ebuf = new StringBuilder

    val proc = Process("""./src/main/python/EvaluatePLFit.py""")
    val pio = new ProcessIO(
      stdin => { stdin.write(dbytes); stdin.close() },
      stdio => { obuf.append(Source.fromInputStream(stdio).getLines.mkString("")) },
      stderr => {
        //ebuf.append(Source.fromInputStream(stderr).getLines.mkString(""))
        //Source.fromInputStream(stderr).getLines.foreach{ x=>println(x) }
      }
    )

    val r = proc.run(pio)
    val f = future{ r.exitValue }
    val pval = runOrDie(timeout, f, () => { println("Killing process"); r.destroy } )

    if ( pval.isDefined && pval.get.asInstanceOf[Int] == 0 ) {
      try {
        val Array(alpha, xmin, ll, p, gof)= obuf.toString().split("\t").map{ x: String => x.toFloat }.toArray
        println("alpha = " + alpha + ", mxin = " + xmin + ", L = " + ll + ", P = " + p + " G = " + gof )
        return (p.toFloat >= 0.1, alpha)
      } catch {
        case e => return (false,-1.0f)
      }
    } else {
      return (false, -1.0f)
    }

  }

  def plotDegreeDist( degs: Seq[Double] ) {
    val dh = new Histogram(degs, nbins = 20)
    val y = dh.hist.map{ v => log(v+1) }
    val x = DenseVector.range(0, y.size).map{ x => log(x+1) }
    val s = DenseVector.ones[Double](y.size) * 0.05

    val labels : PartialFunction[Int,String] = { case i : Int => "" }
    val tips : PartialFunction[Int,String] = {case i : Int => i.toString }
    val literalColors : PartialFunction[Int,Color] = { case x : Int => Color.BLUE }


    scatter( x, y, s, literalColors, labels=labels, tips=tips)
    xlabel( "Histogram bin # (low => high)" )
    ylabel( "log(frequency)" )
  }

  def rankPlotDegrees( degs: Seq[Double], fname: Option[String] = None ) {
    val y = DenseVector(degs.map{ v => log10(v+1) }:_*)
    val x = DenseVector.range(0, y.size).map{ x => log10(x+1) }
    val s = DenseVector.ones[Double](y.size) * 0.01

    val labels : PartialFunction[Int,String] = { case i : Int => "" }
    val tips : PartialFunction[Int,String] = {case i : Int => i.toString }
    val literalColors : PartialFunction[Int,Color] = { case x : Int => Color.BLUE }


    scatter( x, y, s, literalColors, labels=labels, tips=tips)
    xlabel( "log10(node rank)" )
    ylabel( "log10(degree)" )
    if (fname.isDefined) saveas( fname.get )
  }

  def queryContinue() = {
    val r = Dialog.showConfirmation(message="Continue with P-value analysis?")
    r == Result.Ok
  }

  def analyzeSingleProgram( prog: Program, numIt: Int, timeout: Long ): Boolean = {
    val machine = new Machine
    machine.grow(prog, numIt, verbose=true)
    val graph = machine.graph
    println("Graph stats: order="+machine.graph.order+" size="+machine.graph.size)
    val degs = machine.graph.vertices.toSeq.map({v => graph.degree(v).toDouble}).sortWith({(a,b) => a > b})
    plotDegreeDist(degs)
    rankPlotDegrees(degs)
    printProperties(machine.graph)
    println("Clustering: "+clustering(machine.graph))
    val cont = queryContinue()
    if (cont) { return analyzeDegreeDist(degs, timeout)._1 }
    return false
  }

  def analyzePopulation( progs: ArrayBuffer[Program], numIt: Int, timeout: Long, progInd: Int, origGraph: String, progOutFile: String) {
    val machine = new Machine
    var progID = 0
    var max = true

    collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(15)

    val sortedProgs = progs.sortWith({(a,b) =>
      a.properties("fitness").split(",")(0).toDouble > b.properties("fitness").split(",")(0).toDouble})
    println(progs.size)
    var scaleFreeProgs = sortedProgs
    //val scaleFreeProgs = progs
    var alphas = List[Float]()
    /*
    var scaleFreeProgs = sortedProgs.par.filter{
      p =>
        val m = new Machine
        m.grow(p, numIt)
        val degs = m.graph.nodes.toSeq.map({v => v.neighbors.size.toDouble}).sortWith({(a,b) => a > b})
        val (scaleFree, alpha) = analyzeDegreeDist(degs, timeout)
        if (scaleFree) { alphas = alpha :: alphas }
        scaleFree
    }.seq
  */

/*
    val dists = scaleFreeProgs.combinations(2).map{ pair =>
      val (a,b) = (pair(0).shortStr, pair(1).shortStr)
      val ga = new GlobalAlignment(a,b)
      ga.align()
      ga.score
    }.sum
    */

    var i = 0
    scaleFreeProgs.foreach{ p=>
      val q = new Program(p.pcode)

      //val pStr = p.pcode.map{ mop => mop.Str }.mkString("\n")
      println("["+i+"] "+q.length+" , "+p.properties("fitness")+"\n")//+pStr+"\n")
      i += 1
    }


    val objectives = scaleFreeProgs.map{ p=> 
      p.properties("fitness").split(",").map{o=>o.toDouble}
    }

    val numObjectives = objectives(0).size

    println("Number of objectives: "+numObjectives)

    val maxObjectives = (0 until numObjectives).map{ i =>
      (i, (objectives.maxBy{ objArray => objArray(i) }.apply(i) ))
    }.toMap


    println("Max objectives: "+maxObjectives)

    val normedObjectives = objectives.map{objArray=>
      objArray.zipWithIndex.map{ case (v,i) => min(v, 1)/min(maxObjectives(i),1) }//  # /maxObjectives(i)) }
    }

    println("Normalized objectives:")
    normedObjectives.foreach { objArray => println(objArray.mkString(","))}

    def sq(x: Double) = x*x

    def mean(objArray: Array[Double]) = { objArray.sum/objArray.size }

    def stddev(objArray: Array[Double]) = {   
      val meanx  = mean(objArray)
      sqrt(objArray.map{ x => sq(x - meanx) }.sum/objArray.size)
    }

    val (maxObj, maxId) = normedObjectives.zipWithIndex.
      sortBy{ case (objArray, i) => -objArray.sum/stddev(objArray) }.
      apply(0)

    println("Maximum objective: "+maxObj.mkString(",")+"; Program ID: "+maxId)

    var prog = scaleFreeProgs(if(progInd == -1) { maxId } else { progInd })

    def toFile( filename: String, prog: Program ) {
      val fname = "./"+filename
      val file = new File(fname)
      if (!file.exists()) { file.getParentFile().mkdirs() }
      val f = new FileWriter(fname)
      f.write("; GROWCODE\n")
      f.write(prog.pcode.map{ op => op.Str }.mkString("\n"))
      f.close
    }
    toFile(progOutFile, prog)

    //println("Total score: "+dists)
    machine.grow(prog, numIt)//sortedProgs(0), numIt)
    println("Selected Program:")
    println(prog.pcode.map{ mop => mop.Str }.mkString("\n"))
    println("Its fitness characteristics: "+prog.properties("fitness"))
    println("A graph from best model:")
    printProperties(machine.graph)
    println("-----")
    println("Original graph:")
    printProperties(EdgeListReader.fromFileSG(origGraph))
    //println("Mean of alphas"+alphas.sum/alphas.size)
    // scaleFreeProgs.foreach{ prog=>
    //   println("--------------------")
    //   println("Fitness:\t"+prog.properties("fitness").toString)
    //   println("ShortString:\t"+prog.shortStr)
    //   //analyzeSingleProgram(prog, numIt)
    // }

    //println("Analyzing program: "+progInd)
    //var res = analyzeSingleProgram(sortedProgs(progInd), numIt, 50000)
    //println("Scale free!? "+res)
  }

  def main( args: Array[String] ) {

    // Parse the command line options
    var po = new ProgOptions
    po = makeParser(args)

    val isAsciiFile = FileUtils.isAscii(po.progname)
    print(isAsciiFile)
    if (isAsciiFile) {
      analyzeSingleProgram( Program.fromFile(po.progname), po.numIt, po.pTestTimeout)
    } else {
      analyzePopulation( Marshall.read[ArrayBuffer[Program]](po.progname), po.numIt, 
        po.pTestTimeout, po.progInd, po.origGraph,
        po.progOutFile)
    }

  }

}
