package GrowCode
import scala.math._
import scalala.tensor.dense._

object Histogram {
  implicit def toOption[T]( x:T ): Option[T] = { Option(x) }
}

class Histogram( vals: Seq[Double], drange: Option[(Double,Double)] = None, nbins: Int = 10, normed: Boolean = false ) {

  // The bin width.  Simply the bounds of the histogram divided by the number of bins
  private val h = {
    if ( !drange.isEmpty ) {
      (drange.get._2 - drange.get._1) / nbins
    } else {
      (vals.max - vals.min) / nbins
    }
  }

  private val numVal = vals.size
  private val minVal = vals.min
  private val invNormFact = if (normed) { 1.0 / (h*numVal) } else { 1.0 }
  private val b = 1e-10
  private def bin( v:Double ) = { max(0, floor((v - minVal - b) / h).toInt) }

  // Array of values for the histogram bins
  val hist = {
    val h = DenseVector.zeros[Double](nbins)
    // For each value, compute the bin in which it resides and
    // appropriately update that bin's frequency (weight)
    vals.foreach{
      v => h( bin(v) ) += invNormFact
    }
    h
  }

}

/* Old histogram implementation -- unoptimized histogram building
class OldHistogram( vals: Array[Double], drange: Option[(Double,Double)] = None, nbins: Int = 10, normed: Boolean = false ) {
  var svals = vals.sorted
  private val numVal = svals.size

  private val bw = {
    if ( !drange.isEmpty ) {
      (drange.get._2 - drange.get._1) / nbins
    } else {
      (svals( svals.size-1 ) - svals(0)) / nbins
    }
  }

  private val invNormFact = if (normed) { 1.0 / (bw * numVal) } else { 1.0 }

  val bins = (1 to nbins).map{ i => (i * bw)+svals(0) }.toArray
  bins(bins.size-1) += 1e-10
 // bins(0) -= 1e-10
  val mival = vals.head

  // compute the histogram values
  val hist = {
    val h = Array.ofDim[Double](bins.size)

    bins.zipWithIndex.foreach{
      bi =>
      val (l,r) = svals.partition{ x => x < (bi._1) };
      svals = r;
      h(bi._2) = invNormFact * l.size
    }

    h
  }

}
*/
