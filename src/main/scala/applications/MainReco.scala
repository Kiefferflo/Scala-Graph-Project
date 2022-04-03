package applications

object MainReco {
  def main(args: Array[String]): Unit = {
    val INSEE = args(0)
    val (graph,value) = FileReading("./src/main/resources/antennes.csv").AntenneProcessorWithDistance(1.0D / 0.0,INSEE)
    println(graph.minimumSpanningTree(value).toDOTString) //TODO meilleur affichage (dot string ?) c'est compliqué (le graph est trop grand)
  }
}
