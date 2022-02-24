package undirected

import org.scalacheck.Gen
import org.scalacheck.Gen._

/** The test class for [[SimpleGraphDefaultImpl]] implementation */
class SimpleGraphDefaultImplSpec extends SimpleGraphSpec(SimpleGraphDefaultImplSpec)

object SimpleGraphDefaultImplSpec extends SimpleGraphSpecCompanion[Int]("SimpleGraphDefaultImpl") {
    /** @inheritdoc */
    val vertex : Gen[Int] = posNum[Int]

    /** @inheritdoc */
    def graphWithAtLeast(vertexMinCount: Int, edgeMinCount: Int = 0): Gen[SimpleGraphDefaultImpl[Int]] =
      for(vertexAdditionalCount <- posNum[Int] ; vertexCount <- Gen.const((vertexMinCount + vertexAdditionalCount) max 1) ;
          vs <- Gen.containerOfN[Set, Int](vertexCount, vertex) ;
          edgeCount <- Gen.choose(edgeMinCount, vertexCount * (vertexCount - 1) / 2) ;
          es <- Gen.containerOfN[Set, Edge[Int]](edgeCount, edgeFrom(vs))) yield SimpleGraphDefaultImpl(vs, es)
  }
