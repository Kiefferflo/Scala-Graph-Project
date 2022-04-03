package applications

object MainMake {
  def main(args: Array[String]): Unit = {
    val graph = FileReading("./src/main/resources/Makefile").MakeProcessor
    println(graph.toDOTString)
    println(graph.topologicalOrder) //TODO Faire un meilleur affichage
  }
}
