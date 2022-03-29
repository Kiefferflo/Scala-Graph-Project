package directed

/** Implementation of [[StrictGraph]] using list of successors for each vertex
  * @param successors associative map providing set of successors for each vertex
  *                   Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class StrictGraphSuccessorsImpl[V](successors : Map[V, Set[V]]) extends StrictGraph[V] {

    /** @inheritdoc */
    val vertices : Set[V] = successors.keySet

    /** @inheritdoc */
    val arcs : Set[Arc[V]] = (successors foldLeft {Set.empty[Arc[V]]})
    {
        (s,p)=>(p._2 foldLeft s)
        {
            (stemp,v2)=> stemp + Arc(p._1,v2)
        }
    }

    /** @inheritdoc */
    def successorsOf(v: V) : Option[Set[V]] = successors(v) match
    {
        case x if x.nonEmpty=> Some(x)
        case _ => None
    }

    /** @inheritdoc */
    def + (v : V) : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl(successors + (v->Set.empty[V]))

    /** @inheritdoc */
    def - (v : V) : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl(((successors - v) foldLeft Map.empty[V,Set[V]]) {
        (map, pair) => map + (pair._1 -> (pair._2 - v))
    })

    /** @inheritdoc */
    def +| (e: Arc[V]) : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl(
        successors + (e._1->(successors(e._1) + e._2)))

    /** @inheritdoc */
    def -| (e: Arc[V]) : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl(
        successors + (e._1->(successors(e._1) - e._2)))

    /** @inheritdoc */
    def withoutArc : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl(
        (successors foldLeft Map.empty[V,Set[V]])
        {
            (m,p)=>m+ (p._1->Set.empty[V])
        }
    )

    /** @inheritdoc */
    def withAllArcs : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl(
        (successors foldLeft Map.empty[V,Set[V]])
        {
            (m,p)=>m+ (p._1->vertices)
        }
    )
  }

