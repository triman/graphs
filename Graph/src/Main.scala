import ch.dyn.nawak.graphs._
import ch.dyn.nawak.graphs.Node

object Main {

  def main(args: Array[String]): Unit = {
  val n0 = new Node(Some(0))
  val n1 = new Node(Some(1))
  val n2 = new Node(Some(2))
  val n3 = new Node(Some(3))
  val n4 = new Node(Some(4))
  val n5 = new Node(Some(5))
  
  val nodes = List(n0,n1,n2,n3,n4,n5)
  val edges = List[Edge[Int]](
      (n2,n3),
      (n3,n1),
      (n4,n0),
      (n4,n1),
      (n5,n0),
      (n5,n2)
  )
  
  val g2 = new Graph(
	    List[Node[Int]](
	    		0,1,2,3,4,3,5
	        ),
	        List(
	            Edge[Int](0,1),
	            Edge[Int](0,2),
	            Edge[Int](1,3),
	            Edge[Int](4,5)
	        )) with Traversable[Int]
  
  var g = new Graph(nodes, edges)
  
  println(g2 bfsForest)
  
  
  }

}