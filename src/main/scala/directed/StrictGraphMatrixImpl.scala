package directed

/** Implementation of [[StrictGraph]] using adjacency matrix
  * @param vs sequence of vertices in the order they are used in adjacency matrix
  * @param adjacency adjacency matrix
  * @tparam V type for vertices
  */
case class StrictGraphMatrixImpl[V](vs : Seq[V], adjacency : IndexedSeq[IndexedSeq[Int]]) extends StrictGraph[V] {

    /** @inheritdoc */
    lazy val vertices : Set[V] = ???

    /** @inheritdoc */
    lazy val arcs : Set[Arc[V]] = ???

    /** @inheritdoc */
    def successorsOf(v : V) : Option[Set[V]] = ???

    /** @inheritdoc */
    def + (v : V) : StrictGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def - (v : V) : StrictGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def +| (e : Arc[V]) : StrictGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def -| (e : Arc[V]) : StrictGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def withoutArc : StrictGraphMatrixImpl[V] = ???

    /** @inheritdoc */
    def withAllArcs : StrictGraphMatrixImpl[V] = ???
}

