package GrowCode
import no.uib.cipr.matrix.sparse.FlexCompRowMatrix 
import no.uib.cipr.matrix.AbstractMatrix
import no.uib.cipr.matrix.DenseVector
import no.uib.cipr.matrix.Vector.Norm
import scala.util.Random
 
object PowerIteration {
  def leadingEig(A: AbstractMatrix, n: Int, thresh: Double = 1e-3) = {
    var bk = new DenseVector(Array.fill(A.numRows){1.0})
    var bk1 = new DenseVector(A.numRows)
    var i = 0
    var err = Double.MaxValue
    while( i < n && err > thresh) {
      A.mult(bk, bk1)
      val norm = bk1.norm(Norm.Two)
      bk1.scale(1/norm)
      bk.scale(-1)
      bk.add(bk1)
      err = bk.norm(Norm.Two)
      bk.set(bk1)
      i += 1
    }
    bk
  }
}
