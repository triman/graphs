package ch.dyn.nawak.graphs.tests.equality

import org.scalatest._
import ch.dyn.nawak.graphs._

class EdgeEqualitySpec extends FlatSpec with Matchers{
	val e1 = new Edge(new Node(Option(1)), new Node(Option(2)))
	val e2 = new Edge(new Node(Option(1)), new Node(Option(2)))
	
	val e3 = new Edge(new Node(Option(2)), new Node(Option(1)))
	val e4 = new Edge(new Node(Option(2)), new Node(Option(3)))
	
	"Two edges" should "be equals if their extremities are the sames" in {
		e1 should be (e2)
	}
	they should "not be the equals if their extremities are switched" in {
	  	e1 should not be (e3)
	}
	they should "be different if their extremities are different" in {
		e1 should not be (e4)
	}
	
	val we1 = new WeightedEdge(new Node(Option(1)), new Node(Option(2)),1)
	val we2 = new WeightedEdge(new Node(Option(1)), new Node(Option(2)),1)
	
	val we3 = new WeightedEdge(new Node(Option(2)), new Node(Option(1)),1)
	val we4 = new WeightedEdge(new Node(Option(1)), new Node(Option(2)),3)
	
	"A weighted edge" should "be equals to a not-weighted edge if they have the same extremities" in {
		e1 should be (we1)
	}
	
	"Two weighted edges" should "be equals if their extremities are the same and they have the same weight" in {
		we1 should be (we2)
	}
	they should "not be the sabe if their extremities are reverted" in {
		we1 should not be (we3)
	}
	they should "not be equals if they don't have the same weight" in {
		we1 should not be (we4)
	}
}