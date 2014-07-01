package ch.dyn.nawak.graphs

import scala.collection.mutable.MutableList
import scala.collection.mutable.Queue

/**
 * Class used to describe a node (vertex) of the graph
 * @param value Value of the node
 */
case class Node[T](val value : Option[T]){
    def this() = this(None)
        
    override def toString() = value match{
      case Some(value) => "( " + value + " )"
      case None => "None"
    }
}

object Node{
	implicit def T2Node[T](value : T) = new Node[T](Option(value))
	implicit def Node2T[T](value : Node[T]) : T = value.value match{
	  case Some(t) => t
	  case _ => throw new IllegalArgumentException("No value specified for the given node")
	}
}

/**
 * Class used to represent a directed edge of the graph
 * @param start Start node of the edge
 * @param end End node of the edge
 */
case class Edge[T](val start : Node[T], val end : Node[T]){
	override def toString() = start.toString + " -> " + end.toString 
}

object Edge{
	implicit def Tuple2Edge[T](value : Tuple2[Node[T],Node[T]]) = new Edge(value._1, value._2)
}

/**
 * Class to describe a weighted directed edge
 * @see Edge
 */
class WeightedEdge[T,U](start : Node[T],end : Node[T], val weight : U) 
extends Edge[T](start, end){
  
	override def equals(o : Any) = o match {
	  case e : WeightedEdge[T,U] => e.start == start && e.end == end && e.weight == weight
	  case _ => false
	}
  
	override def toString() = start.toString + " - " + weight + " -> " + end.toString
}

object WeightedEdge{
	implicit def Tuple2WeightedEdge[T,U](value : Tuple3[Node[T],Node[T],U]) = new WeightedEdge(value._1, value._2, value._3)
}

/**
 * Trait used to describe a graph using an adjacency list
 */
trait IGraph[T] {
	def nodes : Seq[Node[T]]
	def edges : Seq[Edge[T]]

	// Descriptions
	override def toString() = {
			var s = "["
			  if(! nodes.isEmpty){
			    s += nodes.map(n => n.toString).reduce((a,b) => a + ", " + b)
			  }
			s += "] ["
			if(! edges.isEmpty){
			  s += edges.map(e => e.toString).reduce((a,b) => a + ", " + b)
			}
			s += " ]"
	  s
	}
}

/**
 * Abstract Graph
 */
trait AbstractGraph[T] extends IGraph[T]{}

/**
 * Basic graph implementation, as a return container for most of the functions
 */
class Graph[T] (_nodes : List[Node[T]], _edges : List[Edge[T]]) extends AbstractGraph[T] {
  override def nodes = _nodes
  override def edges = _edges
}

trait WeightedGraph[T,U] extends AbstractGraph[T]{
    override def nodes : Seq[Node[T]]
    override def edges : Seq[WeightedEdge[T,U]]
}

// implementations
class MutableWeightedGraph[T,U] extends /*Graph[T] with*/ WeightedGraph[T,U]{
	import scala.collection.mutable._
  
	var _nodes = MutableList[ch.dyn.nawak.graphs.Node[T]]()
	override def nodes = _nodes
	
	var _edges = MutableList[ch.dyn.nawak.graphs.WeightedEdge[T,U]]()
	override def edges = _edges
}