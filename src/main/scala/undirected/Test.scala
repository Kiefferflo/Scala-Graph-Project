package undirected
import scala.collection.immutable._
object Test {
    def main(args: Array[String]) = {
        val vs = Seq(1,2);
        val a = IndexedSeq(IndexedSeq(0,1),IndexedSeq(1,0))         
         
            /***** Implementation matrix test ****/
        
        val matrixDef = SimpleGraphMatrixImpl(vs,a);
        println(matrixDef.neighborsOf(1)) //ça marche 
        println(matrixDef.vs)
        println("test matrix def +")
        val matrixDef3=matrixDef + 3;
        println(matrixDef3.vs)
        //println(matrixDef3.neighborsOf((3)))
        println("test fini")
        matrixDef - 1;
        println(matrixDef.vs) //pas d'ajout ou de retrait
        val matrixDef1 = matrixDef +| Edge(1,1) 
        println(matrixDef1.neighborsOf(1)) //ajout du voisin 1
        val matrixDef2 = matrixDef1 -| Edge(1,1)
        println(matrixDef2.neighborsOf(1)) //marche

            /***** Implementation Neighbors test ****/
        val n = Map((1,Set(2)),(2,Set(1,3)),(3,Set(2)));
        val ndef = SimpleGraphNeighborsImpl(n)
        println("\n Test de l'implementation par voisin \n")
        println(ndef.neighborsOf(1)) //marche
        println(ndef.neighborsOf(2)) 
        val ndef2= ndef + 4
        println(ndef2.neighbors) //marche
        val ndef3=ndef2- 4
        println(ndef3.neighbors) //marche
        val ndef4 = ndef +| Edge(1,3)
        println(ndef4.neighbors) //marche
        val ndef5 = ndef4 -| Edge(1,3)
        println(ndef5.neighbors) //marche



    } 
}
