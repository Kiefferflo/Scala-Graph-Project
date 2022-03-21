package directed

import scala.annotation.tailrec

/** Trait for a directed ''and strict'' graph, i.e. without loop nor parallel arcs */
trait StrictGraph[V] {
    /* QUERY METHODS */

    /** The set of all vertices of the graph */
    val vertices : Set[V]

    /** The set of all     arcs of the graph */
    val arcs : Set[Arc[V]]

    /** The set of all vertices with arcs incoming from input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the set of all successors of `v` otherwise
      */
    def successorsOf(v : V) : Option[Set[V]]


    /** The number of incoming arcs to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the inner degree of `v` otherwise
      */
    def inDegreeOf(v : V) : Option[Int] =
        if (vertices.contains(v)) Some( // le point entré appartient à la liste des points
            numberOfArcsByDestination(v) //Check from numberOfArcsByDestination

        ) else None


    lazy val numberOfArcsByOrigin : Map[V, Int] =
        arcs groupBy(_._1) map {case(o->ar) => (o->ar.size)}
    lazy val numberOfArcsByDestination : Map[V, Int] =
        arcs groupBy(_._2) map {case(d->ar) => (d->ar.size)}

    /**
    lazy val myPath : Map[V, V] =
        (arcs foldLeft Map.empty[V,V]) {
            (m,ar) => m + (ar._1->ar._2)
            }
    */

    def getArcFromVertice(x1:V,x2:V): Option[Arc[V]] = arcs.find(x => (x._1==x1) && (x._2==x2))




    /** The number of outcoming arcs to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the outer degree of `v` otherwise
      */
    def outDegreeOf(v : V) : Option[Int] =
        if (vertices.contains(v)) Some( // le point entré appartient à la liste des points
            numberOfArcsByOrigin(v) //Check from numberOfArcsByDestination
        ) else None

    /** The number of adjacent vertices to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the degree of `v` otherwise
      */
    def degreeOf(v : V) : Option[Int] = if (vertices.contains(v)) Some( // le point entré appartient à la liste des points
        numberOfArcsByOrigin(v) + numberOfArcsByDestination(v) //TODO WARNING May cause problem if arcs goes both ways
    ) else None

    /* VERTEX OPERATIONS */

    /** Add vertex to graph
      * @param v new vertex
      * @return the graph with new vertex `v`
      *         if `v` is an actual vertex of graph, return input graph
      */
    def + (v : V) : StrictGraph[V]

    /** Remove vertex from graph
      * @param v new vertex
      * @return the graph without vertex `v`
      *         if `v` is not an actual vertex of graph, return input graph
      */
    def - (v : V) : StrictGraph[V]

    /* ARC OPERATIONS */

    /** Add arc to graph (also add arc ends as new vertices if necessary)
      * @param a new arc
      * @return the graph with new arc `e`
      *         if `e` is an actual arc of graph, return input graph
      */
    def +| (a : Arc[V]) : StrictGraph[V]

    /** Remove arc from graph (does NOT remove ends)
      * @param a new arc
      * @return the graph without arc `e`
      *         if `e` is not an actual arc of graph, return input graph
      */
    def -| (a : Arc[V]) : StrictGraph[V]

    /** Remove all arcs from graph but keep same vertices
      * @return graph with same vertices without any arc
      */
    def withoutArc : StrictGraph[V]

    /** Add all possible arc with same vertices
      * @return graph with same vertices and all possible arcs
      */
    def withAllArcs : StrictGraph[V]

    /* SEARCH METHODS */

    /** A topological order of the vertex set (if exists) */
        /** L'ordre topologique existe si le classement topologique est de taille egal au nombre de sommets */
    lazy val topologicalOrder : Option[Seq[V]] =
        if (topologicalBeginning.size==vertices.size) topologicalBeginning else None


    //Créer une fonction recursive: Cherche tous ceux avec out=0
    // Et les mets en memoire V1.
    // Puis, on cherche ceux dont out ne connecte qu'à ceux en V1,
    // On les stock dans V2. Puis V1=V1+V2, et V2 = Void.
    // On recommance jusqu'à ce que V2 = Void
    def topologicalBeginning: Option[Seq[V]] = {

        val myVal = (for (v<-vertices if
          (outDegreeOf(v) match {
              case Some(x) if x== 0=> true
              case Some(x) if x!= 0=> false
              case None => false })) yield v).toSeq
        if (myVal==None) None else topologicalRecursive(Some(myVal))
    }

    /**
     *
     * @param alreadyOrdered Les sommets deja tries
     * @return L'ensemble des sommets que l'on a reussi a trier
     * Afin de fonctionner, on cree NouvValTrie, qui correspond aux sommets suivant a ajouter
     *      Si nouvValTrie est nul, la fonction est finie, sinon, elle "boucle"
     */
    def topologicalRecursive(alreadyOrdered: Option[Seq[V]]): Option[Seq[V]] = {
        val nouvValTrie = (for (v<-vertices if canAddToTopological(v,alreadyOrdered)) yield v).toSeq
        if (nouvValTrie==None) alreadyOrdered else topologicalRecursive(alreadyOrdered+nouvValTrie)
    }

    /**
     *
     * @param v Le sommet a tester
     * @param option Les sommets deja place dans l'analyse topologique
     * @return False si on a deja place le sommet, allDestinationsAlreadyCovered sinon
     */
    def canAddToTopological(v: V, option: Option[Seq[V]]): Boolean = {
        option match {
            case Some(x) => if (x.contains(v)) false else allDestinationsAlreadyCovered(v, x)
            case None => false
        }

    }

    /**
     *
     * @param v Le sommet a tester
     * @param option Les sommets deja tries
     * @return Retourne vrai si tous les sommets pointes par le sommet a tester
     *         sont deja tries, faux sinon
     */
    def allDestinationsAlreadyCovered(v: V, option: Seq[V]): Boolean = {
        val inelegantWay = for (ar<- arcs if ar._1==v) yield option.contains(ar._2)
        //On enregistre tous les arcs partant de v, et si option contient la destination
        !inelegantWay.contains(false)
        //Retourne false si inelegantWay contient false, true sinon
    }

    /**
    * Return True if all elements from the first Seq are in the second
     */

    /* VALUATED GRAPH METHODS */

    /** Computes a shortest path between two vertices
      * @param valuation valuation of graph
      * @param start origin      of path
      * @param end   destination of path
      * @return [[None]] if there is no path from `start` to `end`, the shortest path and its valuation otherwise
      */
    def shortestPath(valuation : Map[Arc[V], Double])(start : V, end : V) : Option[(Seq[V], Double)] = ???
    //La fonction valuation retourne la valeur de l'arc

    def initMyCalculation(valuation : Map[Arc[V], Double])(start:V): Map[V,(V,Double)] = {
        MyCalculation(valuation)(
            (vertices foldLeft Map.empty[V,(V,Double)]) {
                (m,v) => v match {
                    case x if x == start  =>  m + (v->(v,0.0))
                    case _              => m + (v->(v,99999.0))
                }
            },start,0.0)
    }

    def MyCalculation(valuation : Map[Arc[V], Double])(accumulationToHere : Map[V,(V,Double)],currentlyAt:V,currentLength:Double) : Map[V,(V,Double)] = {
        val MyMap  = (accumulationToHere foldLeft Map.empty[V,(V,Double)]) {
            (m,v) => v._2 match {
                case (_,value) if value> valuator(valuation)(currentlyAt,v._1)  =>
                        m + (v._1->(currentlyAt,currentLength + valuator(valuation)(currentlyAt,v._1) ))
                case _              => m + (v)
            }
        }
        if (MyMap!=accumulationToHere) () else MyMap
    }

    def valuator(valuation : Map[Arc[V], Double])(x1:V,x2:V): Double = {
        getArcFromVertice(x1, x2) match {
            case None     => 99999999.0
            case Some(x)      => valuation.get(x) match {
                case Some(value) => value
                case None => 99999999.0
            }
        }
    }

    //Pour shortestPath: definir methode partielle prenant en parametre positionActuelle, valeurActuelle, destination, une map associant un V a un double

    /* toString-LIKE METHODS */

    /** @inheritdoc */
    override lazy val toString : String = s"({${vertices mkString ", "}}, {${arcs mkString ", "}})"

    /** Graph representation in DOT language */
    lazy val toDOTString : String = {
        "strict graph {\n" +
        "    // Edges\n" +
        (arcs foldLeft "    ") { _ + _.toDOTString + "\n    " } + "\n" +
        "    // Vertices\n" +
        vertices.mkString("    ", "\n    ", "\n") +
        "  }\n"
      }

  }
