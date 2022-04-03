package applications

object MainReco {
  def main(args: Array[String]): Unit = {
    val nonInter = args(0).toDouble
    val INSEE = args(1)
    val (graph,value) = FileReading("./src/main/resources/antennes.csv").AntenneProcessorWithDistance(nonInter,INSEE)
    println(graph.minimumSpanningTree(value)) //TODO meilleur affichage (dot string ?)
  }
}
