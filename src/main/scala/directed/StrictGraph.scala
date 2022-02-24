package directed

import scala.annotation.tailrec

/** Trait for a directed ''and strict'' graph, i.e. without loop nor parallel arcs */
trait StrictGraph[V] {
    /* QUERY METHODS */

    /** The set of all vertices of the graph */
    val vertices : Set[V]

    /** The set of all     arcs of the graph */
    val arcs : Set[Arc[V]]

    /** The set of all vertices with arcs incoming from input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the set of all successors of `v` otherwise
      */
    def successorsOf(v : V) : Option[Set[V]]

    /** The number of incoming arcs to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the inner degree of `v` otherwise
      */
    def inDegreeOf(v : V) : Option[Int] = ???

    /** The number of outcoming arcs to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the outer degree of `v` otherwise
      */
    def outDegreeOf(v : V) : Option[Int] = ???

    /** The number of adjacent vertices to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the degree of `v` otherwise
      */
    def degreeOf(v : V) : Option[Int] = ???

    /* VERTEX OPERATIONS */

    /** Add vertex to graph
      * @param v new vertex
      * @return the graph with new vertex `v`
      *         if `v` is an actual vertex of graph, return input graph
      */
    def + (v : V) : StrictGraph[V]

    /** Remove vertex from graph
      * @param v new vertex
      * @return the graph without vertex `v`
      *         if `v` is not an actual vertex of graph, return input graph
      */
    def - (v : V) : StrictGraph[V]

    /* ARC OPERATIONS */

    /** Add arc to graph (also add arc ends as new vertices if necessary)
      * @param a new arc
      * @return the graph with new arc `e`
      *         if `e` is an actual arc of graph, return input graph
      */
    def +| (a : Arc[V]) : StrictGraph[V]

    /** Remove arc from graph (does NOT remove ends)
      * @param a new arc
      * @return the graph without arc `e`
      *         if `e` is not an actual arc of graph, return input graph
      */
    def -| (a : Arc[V]) : StrictGraph[V]

    /** Remove all arcs from graph but keep same vertices
      * @return graph with same vertices without any arc
      */
    def withoutArc : StrictGraph[V]

    /** Add all possible arc with same vertices
      * @return graph with same vertices and all possible arcs
      */
    def withAllArcs : StrictGraph[V]

    /* SEARCH METHODS */

    /** A topological order of the vertex set (if exists) */
    lazy val topologicalOrder : Option[Seq[V]] = ???

    /* VALUATED GRAPH METHODS */

    /** Computes a shortest path between two vertices
      * @param valuation valuation of graph
      * @param start origin      of path
      * @param end   destination of path
      * @return [[None]] if there is no path from `start` to `end`, the shortest path and its valuation otherwise
      */
    def shortestPath(valuation : Map[Arc[V], Double])(start : V, end : V) : Option[(Seq[V], Double)] = ???

    /* toString-LIKE METHODS */

    /** @inheritdoc */
    override lazy val toString : String = s"({${vertices mkString ", "}}, {${arcs mkString ", "}})"

    /** Graph representation in DOT language */
    lazy val toDOTString : String = {
        "strict graph {\n" +
        "    // Edges\n" +
        (arcs foldLeft "    ") { _ + _.toDOTString + "\n    " } + "\n" +
        "    // Vertices\n" +
        vertices.mkString("    ", "\n    ", "\n") +
        "  }\n"
      }

  }
