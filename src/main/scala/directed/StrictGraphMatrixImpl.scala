package directed

/** Implementation of [[StrictGraph]] using adjacency matrix
  * @param vs sequence of vertices in the order they are used in adjacency matrix
  * @param adjacency adjacency matrix
  * @tparam V type for vertices
  */
case class StrictGraphMatrixImpl[V](vs : Seq[V], adjacency : IndexedSeq[IndexedSeq[Int]]) extends StrictGraph[V] {

    /** @inheritdoc */
    lazy val vertices : Set[V] = vs.toSet

    /** @inheritdoc */
    lazy val arcs : Set[Arc[V]] = (vs foldLeft Set.empty[Arc[V]])
    {
        (s,v1) => (vs foldLeft s)
        {
            (s,v2) => if (adjacency(vs.indexOf(v1))(vs.indexOf(v2))==1) s + Arc(v1,v2) else s
        }
    }

    /** @inheritdoc */
    def successorsOf(v : V) : Option[Set[V]] = (arcs foldLeft Set.empty[V]) { (s, a) => if (a._1 == v) s + a._2 else s }
    match {
        case x if x.nonEmpty => Some(x)
        case _ => None
    }

    /** @inheritdoc */
    def + (v : V) : StrictGraphMatrixImpl[V] = StrictGraphMatrixImpl(vs :+ v, (adjacency map { _ :+ 0 } ):+ (for(x<-vs:+v) yield 0).toIndexedSeq)

    /** @inheritdoc */
    def - (v : V) : StrictGraphMatrixImpl[V] = StrictGraphMatrixImpl(vs filterNot { _ == v },
        (adjacency splitAt(vs indexOf v) match {
            case(x,y)=> x :++ (y drop 1)
        })
          map { _ splitAt(vs indexOf v) match {
            case(x,y)=> x :++ (y drop 1)
        }}
    )

    /** @inheritdoc */
    def +| (e : Arc[V]) : StrictGraphMatrixImpl[V] = StrictGraphMatrixImpl(vs,(adjacency.zipWithIndex foldLeft IndexedSeq.empty[IndexedSeq[Int]]) {
        (adj,a)=> adj :+
          (if (vs(a._2)==e._1)
              (a._1.zipWithIndex foldLeft IndexedSeq.empty[Int]) {
                  (adjt,i) =>adjt :+  (if (vs(i._2)==e._2) 1 else i._1)
              }
          else a._1)
    })

    /** @inheritdoc */
    def -| (e : Arc[V]) : StrictGraphMatrixImpl[V] = StrictGraphMatrixImpl(vs,(adjacency.zipWithIndex foldLeft IndexedSeq.empty[IndexedSeq[Int]]) {
        (adj,a)=> adj :+
          (if (vs(a._2)==e._1)
              (a._1.zipWithIndex foldLeft IndexedSeq.empty[Int]) {
                  (adjt,i) =>adjt :+  (if (vs(i._2)==e._2) 0 else i._1)
              }
          else a._1)
    })

    /** @inheritdoc */
    def withoutArc : StrictGraphMatrixImpl[V] = StrictGraphMatrixImpl(vs, adjacency.empty)

    /** @inheritdoc */
    def withAllArcs : StrictGraphMatrixImpl[V] = StrictGraphMatrixImpl(vs,
        (for (x<-vs) yield(for(x<-vs) yield 1).toIndexedSeq).toIndexedSeq)
}

