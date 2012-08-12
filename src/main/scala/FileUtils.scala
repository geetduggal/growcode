package GrowCode
import scalax.io._

object FileUtils {

  def isAscii( fname: String ) = {
    var ascii = false
    val input: Input = Resource.fromFile(fname)
    input.byteArray // read the file into a byte array
    val s = input.slurpString(Codec.UTF8)
    // look for "; GROWCODE"
    if ( s.slice(0,10) == "; GROWCODE" ) { ascii = true }
    ascii
  }

  def isBinary( fname: String ) = { !isAscii(fname) }
}