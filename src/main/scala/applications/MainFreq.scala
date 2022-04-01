package applications

object MainFreq {
  def main(args: Array[String]): Unit = {
    val nonInter = args(1).toDouble
    val INSEE = args(2).toInt
    val graph = FileReading("./src/main/resources/antennes.csv").AntenneProcessor(nonInter,INSEE)
    println(graph.greedyColoring)
  }
}
