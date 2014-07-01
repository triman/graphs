package ch.dyn.nawak.graphs

import scala.collection.mutable.MutableList
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

/**
 * Traversals for a given IGraph
 */
trait Traversable[T] extends IGraph[T] {
	
	trait ContainerView[T]{
		def isEmpty() : Boolean
		def contains(value : T) : Boolean
	}
  
	/**
	 * Used to traverse the graph with a parametrized collection (queue, stack, whatever)
	 * @param start Start node
	 * @param f Function to apply when visiting a node
	 * @param ds Data structure used for the temporary storage of the elements (ex: instance of Queue)
	 * @param i Function used to insert into ds (ex: (ds, element) => ds.push(element) )
	 * @param r Function used to retrieve an element from ds (ex: (ds) => ds.pop() )
	 */
	private def internalTraversal[U <: Seq[Node[T]]](start : Node[T], f : (Node[T]) => Unit, ds : U , i : (U, Seq[Node[T]]) => Unit ,r : (U)=>Node[T]) : IGraph[T] = {
	  val visitedNodes = MutableList[Node[T]]()
	  val visitedEdges = MutableList[Edge[T]]()
	  
	  val toVisit = ds
	  i(ds, List(start))
	  
	  while(!ds.isEmpty){
		  
		  var s = r(ds)
		  visitedNodes += s
		  f(s)
		  var e = edges.filter(e => e.start == s && !visitedNodes.contains(e.end) && !ds.contains(e.end))
		  
		  i(ds,e.map(_.end))
		  e.foreach(e => {
			  visitedEdges += e
		  })
	  }
	  new Graph[T](visitedNodes.toList, visitedEdges.toList)
	}
  
	/**
	 * Build a forest over the current graph. The nodes are taken FIFO (this might have an impact on the resulting forest if the graph isn't symmetrical)
	 * @param f Function used to get the individual trees
	 * @todo check the performances of the current implementation
	 */
	private def internalForest(f : (Traversable[T], Node[T])=>IGraph[T]) : List[IGraph[T]] = {
			val trees = MutableList[IGraph[T]]()
			var t = new Graph(nodes toList, edges toList) with Traversable[T]
			while(!t.nodes.isEmpty){
			  var g = f(t,t.nodes.head)
				trees += g
				val eds = t.edges.filter(e => !(g.nodes.contains(e.start)|| g.nodes.contains(e.end)))
				val nds = t.nodes.filter(n => !g.nodes.contains(n))
				t = new Graph(nds, eds) with Traversable[T]
			}
			trees.toList
	}
	
	/**
	 * BFS search over a graph.
	 * @param start Start node for the traversal
	 * @returns a Graph[T] containing the tree obtained while traversing
	 */
	def bfs(start : Node[T]) : IGraph[T] = bfs(start,_ => {})
	/**
	 * BFS search over a graph.
	 * @param start Start node for the traversal
	 * @param f A function to apply to each node when traversing it
	 * @returns a Graph[T] containing the tree obtained while traversing
	 */
	def bfs(start : Node[T], f : (Node[T]) => Unit) : IGraph[T] =
	  internalTraversal[Queue[Node[T]]](start, f, Queue[Node[T]](), (ds, e) => ds ++= e, ds => ds dequeue )
	
	/**
	 * Build a bfs search forest on the current graph. @see internalForest
	 * @return A List[Graph[T]] containing all the bsf trees.
	 */
	def bfsForest = internalForest((t,n)=>t bfs n)
	
	/**
	 * DFS search over a graph.
	 * @param start Start node for the traversal
	 * @param f A function to apply to each node when traversing it
	 * @returns a Graph[T] containing the tree obtained while traversing
	 */
	def dfs(start : Node[T], f : (Node[T]) => Unit) : IGraph[T] =
	  internalTraversal[Stack[Node[T]]](start, f, Stack[Node[T]](), (ds, e) => ds pushAll e.reverse, ds => ds pop )
	  
	/**
	 * DFS search over a graph.
	 * @param start Start node for the traversal
	 * @returns a Graph[T] containing the tree obtained while traversing
	 */
	def dfs(start : Node[T]) : IGraph[T] = dfs(start,_ => {})
	
	/**
	 * Build a dfs search forest on the current graph. @see internalForest
	 * @return A List[Graph[T]] containing all the bsf trees.
	 */
	def dfsForest = internalForest((t,n)=>t dfs n)
	
	/**
	 * Checks if a connected component, starting at a given node has a cycle in it
	 * @param start Start node for building the connected component
	 * @return the nodes of the connected component along with a boolean flag indicating if it's cyclic
	 */
	def checkCyclicComponent(start : Node[T]) : Tuple2[List[Node[T]],Boolean] = {
	  
		  var graphIsCyclic = false;
	  
		  val visitedNodes = MutableList[Node[T]]()
		  val visitedEdges = MutableList[Edge[T]]()
		  
		  val toVisit = Stack[Node[T]]()
		  toVisit push start
		  
		  while(!toVisit.isEmpty){
			  
			  var s = toVisit.pop
			  var e = edges.filter(e => e.start == s && !visitedNodes.contains(e.end) && !toVisit.contains(e.end))
			  
			  if(edges.count(e => e.start == s && visitedNodes.contains(e.end))> 0){
			    graphIsCyclic = true
			  }
			  
			  visitedNodes += s
			  
			  toVisit pushAll e.map(_.end).reverse
			  e.foreach(e => {
				  visitedEdges += e
			  })
		  }
		  
		  (visitedNodes.toList,graphIsCyclic)
	  }

	
	/**
	 * Checks if the current graph has cycle in it
	 */
	def isCyclic : Boolean = {
	  
	  var t = new Graph(nodes toList, edges toList)
			while(!t.nodes.isEmpty){
			  val component = checkCyclicComponent(t.nodes.head)
				if(component._2){
					return true
				}
			  
				val eds = t.edges.filter(e => !(component._1.contains(e.start)|| component._1.contains(e.end)))
				val nds = t.nodes.filter(n => !component._1.contains(n))
				t = new Graph(nds, eds)
			}
	  
	  false
	}
	/**
	 * Compute the topological sort of the graph.
	 * @return An IGraph containing the topological sort
	 */
	def topologicalSort() : Option[IGraph[T]] = {
		val es = Queue[Edge[T]]() ++= edges
		val s = Queue[Node[T]]() ++= nodes.filter(n => es.count(e => e.end == n) == 0)
		val l = Queue[Node[T]]() 
		
		
		
		while(!(s.isEmpty)){
			val n = s.dequeue
			l.enqueue(n)
			val t = nodes.filter(m => es.count(e => e.start == n && e.end == m) > 0).toList
			println(n)
			println("\t" + t)
			t.foreach(m => {
				es.dequeueAll(e => e.start == n && e.end == m)
				if(es.count(e => e.end == m) == 0)
					s enqueue m
			})
		}
		
		if(es isEmpty){
			Some(new Graph(l toList,edges toList))
		}else{
			None
		}
	}
	
}