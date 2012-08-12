package GrowCode

import MachineOperations._
import net.robpatro.utils.console.ProgressBar
import scala.collection.mutable.{Map, Stack => MStack}
import scala.util.control.Breaks._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.mutable.{Graph => MGraph}
import SlimGraph._
import scala.collection.mutable.HashSet
import scala.math._

class Machine {
  type GraphT = SlimGraphUnDirected[Int, UndirectedEdge[Int]]
  var register = Array.ofDim[Int](3)
  var pc = 0
  var attr = Map.empty[Int, Int]  
  val graph = new SlimGraphUnDirected[Int, UndirectedEdge[Int]]()
  var edgeCache = graph.edges
  val random = util.Random
  val stepCtrs = Map.empty[StepOp, Int]
  val rewindCtrs = Map.empty[Int, Int]
  val stepStack = MStack.empty[(StepOp, Int)]
  val labelMem = Map.empty[Int, Int]
  var halt = false

  val TIME_LIMIT = 1100

  def getRandomNode() = { random.nextInt(graph.order) }

  def grow(prog: Program, numIter: Int, verbose: Boolean = false,
           f: (GraphT) => Unit = (graph: GraphT) => { },
           samp: Int = 25) {

    val mkEdge = (u:Int, v:Int) => UndirectedEdge(u,v)

    graph.insertEdge( mkEdge(0,1) )
    var pb = Option.empty[ProgressBar]
    if (verbose) { pb =  Some(new ProgressBar(numIter, "*"))}

    var x = 0
    while (x < numIter && !halt) {
      //println("Machine["+this+"] %d\t%d\t%d".format(x, graph.order, graph.graphSize))
      if (verbose) { pb.get.update(x) }
      execute(prog)
      //if (verbose) { println(graph.edges.mkString(";")) }
      if (x % samp == 0) {
        f(graph)
        //println(labelMem.size)
        //labelMem.foreach{ case (k,v) =>
        //   println(k+","+v)
        //}
      }
      x += 1
    }
    if (verbose) { pb.get.done() }

  }

  def execute( p: Program ) {
    // Scala's Random.nextInt( n ) has a range of [0,n)
    if (graph.order == 0) { graph.insertVertex(0) }

    register(0) = random.nextInt( graph.order )
    register(1) = random.nextInt( graph.order )
    register(2) = random.nextInt( graph.order )

    var count = 0
    var progLen = p.length
    while ( pc < progLen && count < TIME_LIMIT && !halt ) {
      p.pcode(pc)( this )
      incPC
      count += 1
    }

    reset()
  }

  def reset() = {
    register(0) = 0
    register(1) = 0
    register(2) = 0
    pc = 0
    attr.clear
    stepCtrs.clear
    stepStack.clear
    labelMem.clear
    rewindCtrs.clear
  }

  def nodeIn(i: Int): Int = {
    var x = register(i)
    if ( graph.hasVertex(x) ) { x } else { getRandomNode }
  }

  def incPC() { pc += 1 }

  def stateStr() = {
    "r=[%4d %4d %4d] pc=[%4d] n=[%4d] at=[%d]".format(
      register(0), register(1), register(2),
      pc, graph.order, attr.getOrElse(register(0), 0) )
  }

  def opNop() {}
  def opDetach() {
    val u = nodeIn(0)
    // If you're *somebody's* bitch *and* if you're u's bitch, then remove the edge from the graph
    val bitches = graph.neighborVertices(u).collect{ case v if ( labelMem.isDefinedAt(v) && labelMem(v)==u.value) => UndirectedEdge(u.value,v.value) }
    //println("num bitches: "+bitches.size)
    //graph --= bitches // del my bitches
    bitches.foreach{ bitch => graph.removeEdge(bitch) }
  }

  def opAttach() {
    val u = nodeIn(0)
    val v = nodeIn(1)
    labelMem.foreach { keyval =>
      val (w,label) = keyval
      if (v == label) { graph.insertEdge( UndirectedEdge(u,w) ) }
    }
  }

  def opInfl( p: Float ) {

    var k = register(2)
    val u = nodeIn(0)
    val seen = HashSet.empty[Int]

    // The first level is 1
    var level = 1
    var nextLevel = HashSet.empty[Int]

    if (k == -1) { k = 5 }//Int.MaxValue }

    seen += u
    graph.neighborVertices(u).foreach{ w => nextLevel.add(w) }

    // While there are vertices yet to be visited
    while ( nextLevel.size > 0 && level <= k && !halt ) {
      // The previous round's nextLevel is this round's level
      val thisLevel = nextLevel
      // We create a new set to hold the next round's vertices
      nextLevel = HashSet.empty[Int]
      // For each vertex in this level
      thisLevel.foreach{ v =>
        // If we haven't seen this vertex yet
        if ( !seen.contains(v) && random.nextDouble() < math.pow(p, level) ) {
          labelMem(v) = u
          // Add it's neighbors to the hash for the next level
          graph.neighborVertices(v).foreach{ w => nextLevel.add(w) }
        }
        seen += v
      }
      level += 1
    }

    /*
    if (labelMem.size == 0) {
      println("LabelMem is empty")
    } else {
      labelMem.foreach{ case (node, label) => println("node: "+node+", label: "+label) }
    }
    */
  }

  def opClear() {  register(2) = -1 }

  def opRonPaul() {  labelMem.clear }

  def opCopy() {
    val n = graph.order
    graph.insertVertex(n)
    graph.neighborVertices(nodeIn(0)).foreach{ v => graph.insertEdge( UndirectedEdge(n,v) ) }
    register(1) = nodeIn(0).value
    register(0) = n
  }

  def opNew() {
    val n = graph.order
    graph.insertVertex(n)
    register(0) = n
  }

  def opSave() { register(2) = register(0) }
  def opLoad() { if(register(2) >= 0) { register(0) = register(2) } }

  def opSwap() {
    val x = register(1)
    register(1) = register(0)
    register(0) = x
  }

  def opCreate() {
    if (nodeIn(0).value != nodeIn(1).value) {
      graph.insertEdge( UndirectedEdge(nodeIn(0), nodeIn(1).value) )
    }
  }

  def opRnode() {
    if (graph.order > 1) {
      val x = register(0)
      while (x == register(0)) {
        register(0) = random.nextInt(graph.order)
      }
    }
  }

  def opRedge() {
    if ( graph.size > 0 ) {
      val r = random.nextInt(graph.size)
      // More efficient than edges.toSeq(r)
      val e = graph.edges.slice(r, r+1).head

      // Since there may be a bias in the order in which edge
      // enpoints are stored, randomize which value goes in
      // each register
      if (random.nextInt(2) == 0) {
        register(0) = e.source
        register(1) = e.target
      } else {
        register(1) = e.source
        register(0) = e.target
      }
    }
  }

  def opNeigh() {
    val nlist = graph.neighborVertices(nodeIn(0)).toSeq
    if (nlist.size > 0) {
      register(0) = nlist(random.nextInt(nlist.size))
    }
  }

  def opWalk(n: Int) {
    register(1) = register(0)
    var u = nodeIn(0)
    var i = 0
    var nlist = graph.neighborVertices(u).toSeq
    while (i < n && nlist.size > 0) {
      nlist = graph.neighborVertices(u).toSeq
      u = nlist(random.nextInt(nlist.size))
      i += 1
    }
    register(0) = u
  }

  def opSet(n: Int) {register(2) = n}

  def opStep(stepOp: StepOp) {
    // If we've already encoutered the current step, then just
    // decrement it's counter
    if ( stepCtrs contains stepOp ) {
      stepCtrs( stepOp ) -= 1
    } else {
      // Otherwise, put it in our stepCtrs map and put it on the stepStack
      // as the most recently seen step operation, with the current pc position
      stepCtrs( stepOp ) = stepOp.d
      stepStack.push( (stepOp, pc) )
    }
  }


  def opLoop() {
    // Keep popping steps until we find a valid one or exhaust
    // the viable list
    while( stepStack.nonEmpty ) {

      // Get the syntactically closest valid step
      val (closestStep, pcLoc) = stepStack.pop
      if ( stepCtrs(closestStep) > 0 ){
        pc = pcLoc
        stepStack.push( (closestStep, pcLoc) )
        break
      }

    }
  }

  def opRewind(nInstr: Int, nLoop: Int) {
    if (!rewindCtrs.contains(pc)) {
      rewindCtrs(pc) = nLoop
    }
    if (rewindCtrs(pc) > 0) {
      rewindCtrs(pc) -= 1
      pc = max(pc-nInstr,0)
    }
  }

  def opTruth(p1: Float, p2: Float, p3: Float, p4: Float) {
    val (n0, n1) = ( nodeIn(0), nodeIn(1) );
    val maxIter = 10
    var i = 0
    while( i < maxIter ) {
      val n = getRandomNode().value
      if ( n != n0 && n != n1 ) {

        if (graph.edgeOption(n0,n).isDefined) {
          if (graph.edgeOption(n1,n).isDefined) {
            if (random.nextDouble >= p4) { graph.removeEdge(n0,n) }
          } else if ( random.nextDouble >= p3 ) {
            graph.removeEdge(n0,n)
          }
        } else {
          if (graph.edgeOption(n1,n).isDefined) {
            if (random.nextDouble < p2){ graph.insertEdge( UndirectedEdge(n0,n) ) }
          } else if ( random.nextDouble < p1 ) {
            graph.insertEdge( UndirectedEdge(n0,n) )
          }
        }

      }
      i += 1
    }

  }

  def opScoin(p: Float) { if (random.nextDouble < p) { incPC } }

}
