package undirected

/** Implementation of [[SimpleGraph]] using adjacency matrix
  * @param vs sequence of vertices in the order they are used in adjacency matrix
  * @param adjacency adjacency matrix
  * @tparam V type for vertices
  */
case class SimpleGraphMatrixImpl[V](vs : Seq[V], adjacency : IndexedSeq[IndexedSeq[Int]]) extends SimpleGraph[V] {

    /** @inheritdoc */
    lazy val vertices : Set[V] = ???

    /** @inheritdoc */
    lazy val edges : Set[Edge[V]] = ???

    /** @inheritdoc */
    def neighborsOf(v : V) : Option[Set[V]] = ???

    /** @inheritdoc */
    def + (v : V) : SimpleGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def - (v : V) : SimpleGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def +| (e : Edge[V]) : SimpleGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def -| (e : Edge[V]) : SimpleGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def withoutEdge : SimpleGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def withAllEdges : SimpleGraphMatrixImpl[V] = ???
  }

