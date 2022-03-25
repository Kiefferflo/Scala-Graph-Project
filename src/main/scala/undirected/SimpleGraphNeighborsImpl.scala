package undirected

/** Implementation of [[SimpleGraph]] using list of neighbors for each vertex
  * @param neighbors associative map providing set of neighbors for each vertex
  *                  Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class SimpleGraphNeighborsImpl[V](neighbors : Map[V, Set[V]]) extends SimpleGraph[V] {

    /** @inheritdoc */
    val vertices : Set[V] = neighbors.keySet

    /** @inheritdoc */
    val edges : Set[Edge[V]] = (neighbors foldLeft {Set.empty[Edge[V]]})
    {
      (s,p)=>(p._2 foldLeft s)
      {
        (stemp,v2)=> stemp +Edge(p._1,v2)
      } 
    }

    /** @inheritdoc */
    def neighborsOf(v: V) : Option[Set[V]] = neighbors(v) match
    {
      case x if x.nonEmpty=> Some(x)
      case _ => None
    }

    /** @inheritdoc */
    def + (v : V) : SimpleGraphNeighborsImpl[V] =  SimpleGraphNeighborsImpl(neighbors + (v->Set.empty[V]))

    /** @inheritdoc */
    def - (v : V) : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl(neighbors - v)

    /** @inheritdoc */
    def +| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl((neighbors + (e._1->(neighbors(e._1) + e._2)))+(e._2->(neighbors(e._2) + e._1)) )

    /** @inheritdoc */
    def -| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl((neighbors + (e._1->(neighbors(e._1) - e._2)))+(e._2->(neighbors(e._2) - e._1)) )

    /** @inheritdoc */
    def withoutEdge : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl(
    (neighbors foldLeft Map.empty[V,Set[V]])
    {
      (m,p)=>m+ (p._1->Set.empty[V])
    }
    )

    /** @inheritdoc */
    def withAllEdges : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl(
    (neighbors foldLeft Map.empty[V,Set[V]])
    {
      (m,p)=>m+ (p._1->vertices)
    }
    )
}
