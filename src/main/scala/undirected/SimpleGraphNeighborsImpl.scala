package undirected

/** Implementation of [[SimpleGraph]] using list of neighbors for each vertex
  * @param neighbors associative map providing set of neighbors for each vertex
  *                  Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class SimpleGraphNeighborsImpl[V](neighbors : Map[V, Set[V]]) extends SimpleGraph[V] {

    /** @inheritdoc */
    val vertices : Set[V] = neighbors.keySet ++ neighbors.values.flatten

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
      case x if x != None => Some(x)
      case _ => None
    }

    /** @inheritdoc */
    def + (v : V) : SimpleGraphNeighborsImpl[V] =  SimpleGraphNeighborsImpl(neighbors + (v->Set.empty[V]))

    def parcoursMap[V1,V2](map : Map[V1,V2]) (f : (Map[V1,V2],(V1,V2)) => Map[V1,V2]) : Map[V1,V2] = {
      (map foldLeft Map.empty[V1,V2]) {f}
    }

    /** @inheritdoc */
    def - (v : V) : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl(parcoursMap(neighbors - v)
    {
      (map, pair) => map + (pair._1 -> (pair._2 - v))
    })

    /** @inheritdoc */
    def +| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl((neighbors + (e._1->(neighbors(e._1) + e._2)))+(e._2->(neighbors(e._2) + e._1)) )

    /** @inheritdoc */
    def -| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl((neighbors + (e._1->(neighbors(e._1) - e._2)))+(e._2->(neighbors(e._2) - e._1)) )


    /** @inheritdoc */
    def withoutEdge : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl(
    parcoursMap (neighbors)
    {
      (m,p)=>m+ (p._1->Set.empty[V])
    }
    )

    /** @inheritdoc */
    def withAllEdges : SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl(
    parcoursMap (neighbors)
    {
      (m,p)=>m+ (p._1->vertices)
    }
    )
}
