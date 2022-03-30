package undirected

/** Implementation of [[SimpleGraph]] using adjacency matrix
  * @param vs sequence of vertices in the order they are used in adjacency matrix
  * @param adjacency adjacency matrix
  * @tparam V type for vertices
  */
case class SimpleGraphMatrixImpl[V](vs : Seq[V], adjacency : IndexedSeq[IndexedSeq[Int]]) extends SimpleGraph[V] {

    /** @inheritdoc */
    lazy val vertices : Set[V] = vs.toSet

    /** @inheritdoc */
    lazy val edges : Set[Edge[V]] = (vs foldLeft Set.empty[Edge[V]])
    {
      (s,v1)=>(vs foldLeft s)
      {
        (s,v2)=> 
          if (adjacency(vs.indexOf(v1))(vs.indexOf(v2))==1) 
            s+ Edge(v2,v1) else s
      } 
    }

    

    /** @inheritdoc */
    def neighborsOf(v : V) : Option[Set[V]] = (edges foldLeft Set.empty[V])
      { 
        (s,e)=> e adjacentTo v match
        {
          case Some(x) => s+x
          case None =>s
        } 
      }match {
              case x if x.nonEmpty=> Some(x)
              case _ => None
            }

    /** @inheritdoc */
    def + (v : V) : SimpleGraphMatrixImpl[V] = SimpleGraphMatrixImpl(vs:+v,(adjacency map { _ :+ 0 } ):+ (for(x<-vs:+v) yield 0).toIndexedSeq)

    /** @inheritdoc */
    def - (v : V) : SimpleGraphMatrixImpl[V] = SimpleGraphMatrixImpl(vs filterNot {_ == v }, 
      (adjacency splitAt(vs indexOf v)
        match { 
          case(x,y)=> x :++(y drop 1)
      })
      map {_ splitAt(vs indexOf v) 
        match { 
          case(x,y)=> x :++(y drop 1) 
      }})

    /** @inheritdoc */
    def +| (e : Edge[V]) : SimpleGraphMatrixImpl[V] = SimpleGraphMatrixImpl(
    vs,(adjacency.zipWithIndex foldLeft IndexedSeq.empty[IndexedSeq[Int]]) {
    (adj,a)=> adj :+ 
      (if (vs(a._2)==e._1) 
         (a._1.zipWithIndex foldLeft IndexedSeq.empty[Int]) {
        (adjt,i) =>adjt :+  (if (vs(i._2)==e._2) 1 else i._1)
      }
      else if(vs(a._2)==e._2)
        (a._1.zipWithIndex foldLeft IndexedSeq.empty[Int]) {
        (adjt,i) =>adjt :+  (if (vs(i._2)==e._1) 1 else i._1)
        
      }
      else a._1)
    })

    /** @inheritdoc */
    def -| (e : Edge[V]) : SimpleGraphMatrixImpl[V] =SimpleGraphMatrixImpl(
    vs,(adjacency.zipWithIndex foldLeft IndexedSeq.empty[IndexedSeq[Int]]) {
    (adj,a)=> adj :+ 
      (if (vs(a._2)==e._1) 
         (a._1.zipWithIndex foldLeft IndexedSeq.empty[Int]) {
        (adjt,i) =>adjt :+  (if (vs(i._2)==e._2) 0 else i._1)
      }
      else if(vs(a._2)==e._2)
        (a._1.zipWithIndex foldLeft IndexedSeq.empty[Int]) {
        (adjt,i) =>adjt :+  (if (vs(i._2)==e._1) 0 else i._1)    
      }
      else a._1)
    })

    /** @inheritdoc */
    def withoutEdge : SimpleGraphMatrixImpl[V] = SimpleGraphMatrixImpl(vs,adjacency.empty)

    /** @inheritdoc */
    def withAllEdges : SimpleGraphMatrixImpl[V] = SimpleGraphMatrixImpl(
    vs, (for (x<-vs) yield(for(x<-vs) yield 1).toIndexedSeq).toIndexedSeq)
  }

