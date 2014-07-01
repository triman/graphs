package ch.dyn.nawak.graphs.tests.traversals

import org.scalatest._
import ch.dyn.nawak.graphs._

class BfsSpec  extends FlatSpec with Matchers {
	val g = new Graph(
	    List[Node[Int]](
	    		0,1,2,3
	        ),
	        List(
	            Edge[Int](0,1),
	            Edge[Int](0,2),
	            Edge[Int](1,2),
	            Edge[Int](2,0),
	            Edge[Int](2,3),
	            Edge[Int](3,3)
	        )) with Traversable[Int]
	
	"BFS for the graph " + g + " starting from (2)" should "have nodes in order 2,0,3,1" in {
		val bfsResult = g bfs 2
		bfsResult shouldBe a [IGraph[Int]]
		bfsResult.nodes should contain theSameElementsInOrderAs List[Node[Int]](2,0,3,1) 
	}
	
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
	"BFS for the graph " + g2 + " starting from (0)" should "be 0,1,2,3" in {
		val bfsResult = g2 bfs 0
		bfsResult.nodes should contain theSameElementsInOrderAs List[Node[Int]](0,1,2,3) 
	}
	
	"BFS Forest for the graph " + g2 + " starting from (0)" should "be a list of 2 graphs" in {
		val bfsResult = g2.bfsForest
		bfsResult shouldBe a [List[IGraph[Int]]]
		bfsResult.length should be (2)
	}
}