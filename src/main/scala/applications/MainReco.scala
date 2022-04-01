package applications

object MainReco {
  def main(args: Array[String]): Unit = {
    val nonInter = args(1).toDouble
    val INSEE = args(2).toInt
    val (graph,value) = FileReading("./src/main/resources/antennes.csv").AntenneProcessorWithDistance(nonInter,INSEE)
    println(graph.minimumSpanningTree(value))
  }
}
