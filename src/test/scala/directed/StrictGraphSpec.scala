package directed

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen._
import org.scalacheck.Gen

/** General template for test of [[StrictGraph]] implementations
  * @param companion the companion object of test class (see [[StrictGraphSpecCompanion]] documentation)
  */
abstract class StrictGraphSpec[V](companion : StrictGraphSpecCompanion[V]) extends AnyFlatSpec with Matchers {
    import companion._

    /* QUERY METHODS */

    behavior of s"$name.successorsOf"
      it must "yield None if input is not an actual vertex of graph" in
        forAll(graphAndNoActualVertex) { case (g, v) => (g successorsOf v) mustBe None }
      it must "yield a subset of vertex set if input is an actual vertex of graph" in
        forAll(graphAndActualVertex)   { case (g, v) => (g successorsOf v) map { _ subsetOf g.vertices } must equal (Some(true)) }
      it must "yield a set of adjacent vertices" in
        forAll(graphAndActualVertex)   { case (g, v) => (g successorsOf v).get forall { g.arcs contains Arc(v, _) }}

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
      it must "have input vertex removed from successors if an actual vertex of graph" in
        forAll(graphAndActualVertex)   { case (g, v) => (g successorsOf v).get forall { n => !((g - v).successorsOf(n).get contains v) } }
      it must "have degree updated for all successors if an actual vertex of graph" in
        forAll(graphAndActualVertex)   { case (g, v) => (g successorsOf v).get forall { n => (g - v).degreeOf(n) contains (g.degreeOf(n).get - 1) }}
      it must "revert back the + operator applied on an actual new vertex" in
        forAll(graphAndNoActualVertex) { case (g, v) => (g + v) - v must equal (g) }

    /* ARC OPERATIONS */

    behavior of s"$name.+|"
      it must "contain input arc" in
        forAll(zip(graph, arc))   { case (g, a) => (g +| a).arcs must equal (g.arcs + a) }
      it must "contain ends of input arc" in
        forAll(zip(graph, arc))   { case (g, a) => Set(a._1, a._2) subsetOf (g +| a).vertices mustBe true }
      it must "return input graph if input arc is already in graph" in
        forAll(graphAndActualArc) { case (g, a) =>  g +| a       must equal   (g) }

    behavior of s"$name.-|"
      it must "NOT contain input arc" in
        forAll(zip(graph, arc))     { case (g, a) => (g -| a).arcs must equal (g.arcs - a) }
      it must "return input graph if input arc is not already in graph" in
        forAll(graphAndNoActualArc) { case (g, a) =>  g -| a       must equal (g) }
      it must "have ends removed from successors if an actual vertex of graph" in
        forAll(graphAndActualArc)   { case (g, a) => (g -| a).successorsOf(a._1) must not contain a._2 }
      it must "have degree updated for both ends if an actual vertex of graph" in
        forAll(graphAndActualArc)   { case (g, a) => (g degreeOf a._1 map { _ - 1}, g degreeOf a._2 map { _ - 1}) must equal ((g -| a) degreeOf a._1, (g -| a) degreeOf a._2) }

    behavior of s"$name.withoutArc"
      it must "let vertex set unchanged" in
        forAll(graph) { g =>  g.withoutArc.vertices must equal (g.vertices) }
      it must "have empty arc set" in
        forAll(graph) { g =>  g.withoutArc.arcs mustBe empty }
      it must "have empty successors for any vertex" in
        forAll(graph) { g => (g.withoutArc.vertices forall { v => (g.withoutArc successorsOf v) == Some(Set.empty[V]) }) mustBe true }
      it must "have zero degree for any vertex" in
        forAll(graph) { g => (g.withoutArc.vertices forall { v => (g.withoutArc degreeOf v) == Some(0) }) mustBe true }
  }

/** General template for [[StrictGraphSpec]] companion objects
  * @param name name of actual implementation
  * @tparam V type of vertex used for test
  */
abstract class StrictGraphSpecCompanion[V](val name : String) {
    /* BASIC GENERATORS */

    /** The vertex generator */
    val vertex : Gen[V]

    /** The arc generator
      * @param vertices set of vertices to pick from
      *                 (if empty, generator [[vertex]] is used)
      */
    def arcFrom(vertices : Set[V]) : Gen[Arc[V]] = {
        val vertexGenerator = if (vertices.isEmpty) vertex else Gen.oneOf(vertices)
        for (v1 <- vertexGenerator ; v2 <- vertexGenerator if v1 != v2) yield Arc(v1, v2)
      }

    /** The arc generator */
    lazy val arc : Gen[Arc[V]] = arcFrom(Set.empty[V])

    /** The undirected simple graph generator
      *
      * If `g` is generated value, then it must be ensured that `g.vertices` is NOT empty.
      */
    val graph : Gen[StrictGraph[V]] = graphWithAtLeast(1)

    /** The general directed simple graph generator
      *
      * If `g` is generated value, then it must be ensured that `g.vertices` have at least `vertexMinCount` elements
      *                                                     and `g.arcs`    have at least   `arcMinCount` elements
      * @param vertexMinCount minimal number of vertices to use
      * @param arcMinCount   minimal number of arcs to use
      */
    def graphWithAtLeast(vertexMinCount : Int, arcMinCount : Int = 0) : Gen[StrictGraph[V]]

    /** A positive-valued valuation generator
      * @param arcs set of whose values must be defined
      */
    def positiveValuation(arcs : Set[Arc[V]]) : Gen[Map[Arc[V], Double]] =
      for (values <- Gen.containerOfN[Seq, Double](arcs.size, posNum[Double])) yield (arcs.toSeq zip (values map {_ + 1})).to(Map)

    /* ADVANCED GENERATORS */

    /** A generator for a graph and a vertex of the graph
      *
      * If `(g, v)` is a generated value, then it must be ensured that `g.vertices` contains `v`.
      */
    lazy val graphAndActualVertex : Gen[(StrictGraph[V], V)] =
      for (g <- graph ; v <- Gen.oneOf(g.vertices)) yield (g, v)

    /** A generator for a graph and a vertex NOT in the graph
      *
      * If `(g, v)` is a generated value, then it must be ensured that `g.vertices` does not contain `v`.
      */
    lazy val graphAndNoActualVertex : Gen[(StrictGraph[V], V)] =
      zip(graph, vertex) suchThat { case (g, v) => !(g.vertices contains v) }

  /** A generator for a graph and an arc of the graph
      *
      * If `(g, a)` is a generated value, then it must be ensured that `g.arcs`    contains `a`.
      */
    lazy val graphAndActualArc : Gen[(StrictGraph[V], Arc[V])] =
      for (g <- graphWithAtLeast(2, 1) ; a <- Gen.oneOf(g.arcs)) yield (g, a)

    /** A generator for a graph and an arc NOT in the graph
      *
      * If `(g, a)` is a generated value, then it must be ensured that `g.arcs` does not contain `a`.
      */
    lazy val graphAndNoActualArc : Gen[(StrictGraph[V], Arc[V])] =
      for (g <- graph ; a <- arcFrom(g.vertices) if !(g.arcs contains a)) yield (g, a)

    /** A generator for a graph and a positive valuation */
    lazy val graphAndValuation : Gen[(StrictGraph[V], Map[Arc[V], Double])] =
      for (g <- graphWithAtLeast(2, 1) ; v <- positiveValuation(g.arcs)) yield (g, v)
  }
