package directed

/** Implementation of [[StrictGraph]] using list of successors for each vertex
  * @param successors associative map providing set of successors for each vertex
  *                   Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class StrictGraphSuccessorsImpl[V](successors : Map[V, Set[V]]) extends StrictGraph[V] {

    /** @inheritdoc */
    val vertices : Set[V] = ???

    /** @inheritdoc */
    val arcs : Set[Arc[V]] = ???

    /** @inheritdoc */
    def successorsOf(v: V) : Option[Set[V]] = ???

    /** @inheritdoc */
    def + (v : V) : StrictGraphSuccessorsImpl[V] = ???

    /** @inheritdoc */
    def - (v : V) : StrictGraphSuccessorsImpl[V] = ???

    /** @inheritdoc */
    def +| (e: Arc[V]) : StrictGraphSuccessorsImpl[V] = ???

    /** @inheritdoc */
    def -| (e: Arc[V]) : StrictGraphSuccessorsImpl[V] = ???

    /** @inheritdoc */
    def withoutArc : StrictGraphSuccessorsImpl[V] = ???

    /** @inheritdoc */
    def withAllArcs : StrictGraphSuccessorsImpl[V] = ???
  }

