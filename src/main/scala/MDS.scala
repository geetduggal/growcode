package GrowCode
import scalala.tensor.dense._
import scalala.library.LinearAlgebra._
import scalala.tensor.::
import scalala.operators._
import scalala.tensor.DiagonalMatrix

object MDS {
  def sq(x: Double) = { x*x }

  def classicalMDS(D: DenseMatrix[Double], k: Int) = {
    val n = D.numRows
    val J = DenseMatrix.eye[Double](n) - DenseMatrix.ones[Double](n,n)*1/n.toDouble
    val B = J*D*J*(-0.5)
    val (r,i,v) = eig(B)
    val s = r.map({ x => if (x<1e-10) {0} else {x}})
    val order = s.argsort.reverse
    val w = v(order,::)
    val sqErr = s(order):^2.0
    val error = sqErr.sum - sqErr(0 until k).sum
    val q = w(::,0 until k)
    val l = diag(s(0 until k):^0.5)
    (q*l, error)
  }
}
