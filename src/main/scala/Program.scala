package GrowCode
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scala.collection.mutable.Map
import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, Map => MMap }
import MachineOperations._

/**
 * Companion Object for Program class
 */
object Program {

  /**
   * Read a growcode program frome file and return
   * a new Program object representing that program.
   */
  def fromFile( filename:String ) = {
    val cbuffer = ArrayBuffer.empty[MachineOperation]
    cbuffer += StepOp(1)
    val fsrc = Source.fromFile( filename )

    fsrc.getLines.foreach{
      l =>
        val cidx = l.indexOf(';')
        val line = (if ( cidx != -1 ){ l.slice(0,cidx) } else { l }).toLowerCase
        if (line != "") {
          val sline = line.dropWhile{x=>x==' ' || x=='\t'}
          val toks = sline.split("""\s+""").toList
          println(toks)
          if( toks.size > 0) {
            cbuffer += MachineOperation.fromTokenList(toks)
          }
        }
    }

    fsrc.close()
    cbuffer += LoopOp()
    val p = new Program( cbuffer.toArray )
    p
  }


}


class Program ( val pcode: Array[MachineOperation] ) extends Serializable {

  val properties = MMap.empty[String, String]

  def shortStr = { pcode.map{ mop => mop.Str }.mkString("\n") }
  //def shortStr = { pcode.map{ mop => mop.shortStr }.mkString("") }
  def length = { pcode.size }
  def instr( pc: Int ) = { pcode(pc) }

}
