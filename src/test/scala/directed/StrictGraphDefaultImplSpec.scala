package directed

import org.scalacheck.Gen
import org.scalacheck.Gen._

/** The test class for [[StrictGraphDefaultImpl]] implementation */
class StrictGraphDefaultImplSpec extends StrictGraphSpec(StrictGraphDefaultImplSpec)

object StrictGraphDefaultImplSpec extends StrictGraphSpecCompanion[Int]("StrictGraphDefaultImpl") {
    /** @inheritdoc */
    val vertex : Gen[Int] = posNum[Int]

    /** @inheritdoc */
    def graphWithAtLeast(vertexMinCount: Int, arcMinCount: Int = 0): Gen[StrictGraphDefaultImpl[Int]] =
      for(vertexAdditionalCount <- posNum[Int]; vertexCount <- Gen.const((vertexMinCount + vertexAdditionalCount) max 1);
          vs <- Gen.containerOfN[Set, Int](vertexCount, vertex);
          arcCount <- Gen.choose(arcMinCount, vertexCount * (vertexCount - 1) / 2);
          as <- Gen.containerOfN[Set, Arc[Int]](arcCount, arcFrom(vs))) yield StrictGraphDefaultImpl(vs, as)
  }
