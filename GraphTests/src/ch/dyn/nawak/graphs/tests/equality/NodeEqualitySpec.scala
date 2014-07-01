package ch.dyn.nawak.graphs.tests.equality

import org.scalatest._
import ch.dyn.nawak.graphs._

class NodeEqualitySpec extends FlatSpec with Matchers {
	val n1 = new Node[Int](Option(1))
	val n2 = new Node[Int](Option(1))
	
	val n3 = new Node[Int](Option(3))
	val emptyNode = new Node[Int](None)
	val emptyNode2 = new Node[Int]()
	
	"Two nodes" should "be equals if they have the same value" in {
		n1 should be (n2)
	}
	
	they should "not be equals if they have different values" in {
		n1 should not be (n3)
		n1 should not be (emptyNode)
	}
	
	they should "be equals if both are empty" in {
		emptyNode should be (emptyNode2)
	}
	
	they should "have the same hashcode when equals" in {
		n1.hashCode() should be (n2.hashCode())
		n1.hashCode() should not be (n3.hashCode())
	}
}