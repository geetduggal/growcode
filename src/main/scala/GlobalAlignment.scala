package GrowCode
import scala.math._
import scalala.tensor.dense._

class GlobalAlignment( a:String, b:String ) {

  var table = DenseMatrix.zeros[Double](a.length+1, b.length+1)
  var traceback = DenseMatrix.zeros[Int](a.length+1, b.length+1)
  var choices = DenseVector.zeros[Double](3)

  def align() {

    (1 to a.length).foreach{ i=> table(i,0) = i }
    (1 to b.length).foreach{ j=> table(0,j) = j }

    (1 to a.length).foreach{ i=>
      (1 to b.length).foreach { j=>
        val costMatch = if (a(i-1) == b(j-1)) {-1} else {1}

        choices(0) =  costMatch + table(i-1, j-1) // extend
        choices(1) = 1 + table(i-1, j) // gap in b
        choices(2) = 1 + table(i, j-1) // gap in a

        table(i,j) = choices.min


      traceback(i,j) =
        if (table(i,j) == choices(0)) {0}
        else if  (table(i,j) == choices(1)) {1}
        else {2}

      }
    }
  }

  def score = { table(a.length, b.length) }

  def printAlignment() {
    var aAligned = new StringBuilder()
    var bAligned = new StringBuilder()
    var alignMap = new StringBuilder()

    var i = a.length
    var j = b.length

    while (i != 0 && j != 0) {

      if (traceback(i,j) == 0) {
        aAligned.append(a(i-1))
        bAligned.append(b(j-1))
        i -= 1
        j -= 1
      }

      else if (traceback(i,j) == 1) {
        aAligned.append(a(i-1))
        bAligned.append('-')
        i -= 1
      }

      else {
        aAligned.append('-')
        bAligned.append(b(j-1))
        j -= 1
      }
    }

    println(aAligned.reverse)
    println(bAligned.reverse)

  }

}
