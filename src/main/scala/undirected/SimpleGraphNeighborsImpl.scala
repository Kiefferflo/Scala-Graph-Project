package undirected

/** Implementation of [[SimpleGraph]] using list of neighbors for each vertex
  * @param neighbors associative map providing set of neighbors for each vertex
  *                  Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class SimpleGraphNeighborsImpl[V](neighbors : Map[V, Set[V]]) extends SimpleGraph[V] {

    /** @inheritdoc */
    val vertices : Set[V] = ???

    /** @inheritdoc */
    val edges : Set[Edge[V]] = ???

    /** @inheritdoc */
    def neighborsOf(v: V) : Option[Set[V]] = ???

    /** @inheritdoc */
    def + (v : V) : SimpleGraphNeighborsImpl[V] = ???

    /** @inheritdoc */
    def - (v : V) : SimpleGraphNeighborsImpl[V] = ???

    /** @inheritdoc */
    def +| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] = ???

    /** @inheritdoc */
    def -| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] = ???

    /** @inheritdoc */
    def withoutEdge : SimpleGraphNeighborsImpl[V] = ???

    /** @inheritdoc */
    def withAllEdges : SimpleGraphNeighborsImpl[V] = ???
}
