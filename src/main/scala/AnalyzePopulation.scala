package GrowCode
import scala.math._
import scala.collection.mutable.ArrayBuffer
import scalala.tensor.dense._
import scalala.tensor.::
import scalala.library.Plotting._
import scalala.library.plotting.GradientPaintScale
import java.awt.Color

object AnalyzePopulation {

  def main( args:Array[String] ) {
    val progs = Marshall.read[ ArrayBuffer[Program] ]( args(0) )
    val shortStrings = progs.map{ p => p.shortStr }
    
    val fitness = progs.map{ p => p.properties("fitness").toDouble }
    val maxFit = abs(fitness.min)
    val scaledFitness = fitness.map{ f => 1.0 - (abs(f)/maxFit) }

    val stringDists = DenseMatrix.zeros[Double](50,50)
    for( x <- 0 until 50; y <- x until 50 ){
      val (a,b) = (shortStrings(x), shortStrings(y))
      val ga = new GlobalAlignment(a,b)
      ga.align()
      stringDists(x,y) = ga.score
      stringDists(y,x) = ga.score
    }

    val (embed, error) = MDS.classicalMDS(stringDists,2)

    val y = embed(::,0)
    val x = embed(::,1)
    val s = DenseVector.ones[Double](y.size) * 0.025

    val labels : PartialFunction[Int,String] = { case i : Int => "" }
    val tips : PartialFunction[Int,String] = {case i : Int => fitness(i).toString }
    //val literalColors : PartialFunction[Int,Color] = { case x : Int => Color.BLUE }

    val paintScale = GradientPaintScale(0.0, 1.0)
    val scaleColors = Map() ++ (0 until fitness.length).map(i => (i, paintScale(scaledFitness(i))))

    scatter( x, y, s, scaleColors, labels=labels, tips=tips)
  }

}
