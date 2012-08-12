import scala.io._
import scala.collection.mutable.Set
import scala.collection.mutable.Map

object EdgeNamesToID {

  case class Edge(v: String, w: String)
  
  def main(args: Array[String]) {
 
    val nodes = Set[String]() 
    val edges = Set[Edge]()

    Source.fromFile(args(0)).getLines().foreach { x =>
      var Array(v,w) = x.split("""\s+""")
      if (v > w) {val tmp = v; v = w; w = tmp }
      nodes += v 
      nodes += w
      edges += new Edge(v,w)
    }

    val node2Int = nodes.toSeq.zipWithIndex.toMap
   
    edges.foreach { edg =>
      println(node2Int(edg.v)+" "+node2Int(edg.w))
    } 

    println("# nodes: "+nodes.size+","+"# edges: "+edges.size)

  }
}
