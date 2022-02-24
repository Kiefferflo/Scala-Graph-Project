package undirected

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen._
import org.scalacheck.Gen

/** General template for test of [[SimpleGraph]] implementations
  * @param companion the companion object of test class (see [[SimpleGraphSpecCompanion]] documentation)
  */
abstract class SimpleGraphSpec[V](companion : SimpleGraphSpecCompanion[V]) extends AnyFlatSpec with Matchers {
    import companion._

    /* QUERY METHODS */

    behavior of s"$name.neighborsOf"
      it must "yield None if input is not an actual vertex of graph" in
        forAll(graphAndNoActualVertex) { case (g, v) => (g neighborsOf v) mustBe None }
      it must "yield a subset of vertex set if input is an actual vertex of graph" in
        forAll(graphAndActualVertex)   { case (g, v) => (g neighborsOf v) map { _ subsetOf g.vertices } must equal (Some(true)) }
      it must "yield a set of adjacent vertices" in
        forAll(graphAndActualVertex)   { case (g, v) => (g neighborsOf v).get forall { g.edges contains Edge(v, _) }}

    /* VERTEX OPERATIONS */

    behavior of s"$name.+"
      it must "contain input vertex" in
        forAll(zip(graph, vertex))     { case (g, v) => (g + v).vertices must equal (g.vertices + v) }
      it must "return input graph if input vertex is already in graph" in
        forAll(graphAndActualVertex)   { case (g, v) =>  g + v           must equal   (g) }
      it must "have input vertex with degree 0 if not an actual vertex of graph" in
        forAll(graphAndNoActualVertex) { case (g, v) => (g + v) degreeOf v mustBe Some(0) }

    behavior of s"$name.-"
      it must "NOT contain input vertex" in
        forAll(zip(graph, vertex))     { case (g, v) => (g - v).vertices must equal (g.vertices - v) }
      it must "return input graph if input vertex is not already in graph" in
        forAll(graphAndNoActualVertex) { case (g, v) =>  g - v      must equal (g) }
      it must "have input vertex removed from neighbors if an actual vertex of graph" in
        forAll(graphAndActualVertex)   { case (g, v) => (g neighborsOf v).get forall { n => !((g - v).neighborsOf(n).get contains v) } }
      it must "have degree updated for all neighbors if an actual vertex of graph" in
        forAll(graphAndActualVertex)   { case (g, v) => (g neighborsOf v).get forall { n => (g - v).degreeOf(n) contains (g.degreeOf(n).get - 1) }}
      it must "revert back the + operator applied on an actual new vertex" in
        forAll(graphAndNoActualVertex) { case (g, v) => (g + v) - v must equal (g) }

    /* EDGE OPERATIONS */

    behavior of s"$name.+|"
      it must "contain input edge" in
        forAll(zip(graph, edge))   { case (g, e) => (g +| e).edges must equal (g.edges + e) }
      it must "contain ends of input edge" in
        forAll(zip(graph, edge))   { case (g, e) => Set(e._1, e._2) subsetOf (g +| e).vertices mustBe true }
      it must "return input graph if input edge is already in graph" in
        forAll(graphAndActualEdge) { case (g, e) =>  g +| e         must equal   (g) }

    behavior of s"$name.-|"
      it must "NOT contain input edge" in
        forAll(zip(graph, edge))     { case (g, e) => (g -| e).edges must equal (g.edges - e) }
      it must "return input graph if input edge is not already in graph" in
        forAll(graphAndNoActualEdge) { case (g, e) =>  g -| e        must equal (g) }
      it must "have ends removed from neighbors if an actual vertex of graph" in
        forAll(graphAndActualEdge)   { case (g, e) => (g -| e).neighborsOf(e._1) must not contain e._2 }
      it must "have degree updated for both ends if an actual vertex of graph" in
        forAll(graphAndActualEdge)   { case (g, e) => (g degreeOf e._1 map { _ - 1}, g degreeOf e._2 map { _ - 1}) must equal ((g -| e) degreeOf e._1, (g -| e) degreeOf e._2) }

    behavior of s"$name.withoutEdge"
      it must "let vertex set unchanged" in
        forAll(graph) { g =>  g.withoutEdge.vertices must equal (g.vertices) }
      it must "have empty edge set" in
        forAll(graph) { g =>  g.withoutEdge.edges mustBe empty }
      it must "have empty neighbors for any vertex" in
        forAll(graph) { g => (g.withoutEdge.vertices forall { v => (g.withoutEdge neighborsOf v) == Some(Set.empty[V]) }) mustBe true }
      it must "have zero degree for any vertex" in
        forAll(graph) { g => (g.withoutEdge.vertices forall { v => (g.withoutEdge degreeOf v) == Some(0) }) mustBe true }
  }

/** General template for [[SimpleGraphSpec]] companion objects
  * @param name name of actual implementation
  * @tparam V type of vertex used for test
  */
abstract class SimpleGraphSpecCompanion[V](val name : String) {
    /* BASIC GENERATORS */

    /** The vertex generator */
    val vertex : Gen[V]

    /** The edge generator
      * @param vertices set of vertices to pick from
      *                 (if empty, generator [[vertex]] is used)
      */
    def edgeFrom(vertices : Set[V]) : Gen[Edge[V]] = {
        val vertexGenerator = if (vertices.isEmpty) vertex else Gen.oneOf(vertices)
        for (v1 <- vertexGenerator ; v2 <- vertexGenerator if v1 != v2) yield Edge(v1, v2)
      }

    /** The edge generator */
    lazy val edge : Gen[Edge[V]] = edgeFrom(Set.empty[V])

    /** The undirected simple graph generator
      *
      * If `g` is generated value, then it must be ensured that `g.vertices` is NOT empty.
      */
    val graph : Gen[SimpleGraph[V]] = graphWithAtLeast(1)

    /** The general directed simple graph generator
      *
      * If `g` is generated value, then it must be ensured that `g.vertices` have at least `vertexMinCount` elements
      *                                                     and `g.edges`    have at least   `edgeMinCount` elements
      * @param vertexMinCount minimal number of vertices to use
      * @param edgeMinCount   minimal number of edges to use
      */
    def graphWithAtLeast(vertexMinCount : Int, edgeMinCount : Int = 0) : Gen[SimpleGraph[V]]

    /** A positive-valued valuation generator
      * @param edges set of whose values must be defined
      */
    def positiveValuation(edges : Set[Edge[V]]) : Gen[Map[Edge[V], Double]] =
      for (values <- Gen.containerOfN[Seq, Double](edges.size, posNum[Double])) yield (edges.toSeq zip (values map {_ + 1})).to(Map)

  /* ADVANCED GENERATORS */

    /** A generator for a graph and a vertex of the graph
      *
      * If `(g, v)` is a generated value, then it must be ensured that `g.vertices` contains `v`.
      */
    lazy val graphAndActualVertex : Gen[(SimpleGraph[V], V)] =
      for (g <- graph ; v <- Gen.oneOf(g.vertices)) yield (g, v)

    /** A generator for a graph and a vertex NOT in the graph
      *
      * If `(g, v)` is a generated value, then it must be ensured that `g.vertices` does not contain `v`.
      */
    lazy val graphAndNoActualVertex : Gen[(SimpleGraph[V], V)] =
      for (g <- graph ; v <- vertex if !(g.vertices contains v)) yield (g, v)

    /** A generator for a graph and an edge of the graph
      *
      * If `(g, e)` is a generated value, then it must be ensured that `g.edges`    contains `e`.
      */
    lazy val graphAndActualEdge : Gen[(SimpleGraph[V], Edge[V])] =
      for (g <- graphWithAtLeast(2, 1) ; e <- Gen.oneOf(g.edges)) yield (g, e)

    /** A generator for a graph and an edge NOT in the graph
      *
      * If `(g, e)` is a generated value, then it must be ensured that `g.edges` does not contain `e`.
      */
    lazy val graphAndNoActualEdge : Gen[(SimpleGraph[V], Edge[V])] =
      for (g <- graph ; e <- edgeFrom(g.vertices) if !(g.edges contains e)) yield (g, e)

    /** A generator for a graph and a positive valuation */
    lazy val graphAndValuation : Gen[(SimpleGraph[V], Map[Edge[V], Double])] =
      for (g <- graphWithAtLeast(2, 1) ; v <- positiveValuation(g.edges)) yield (g, v)
  }
