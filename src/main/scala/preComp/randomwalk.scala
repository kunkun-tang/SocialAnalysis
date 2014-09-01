package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.HashSet
import scala.util.Random
import scala.math
import probabilitymonad.Distribution._

object RWM{

  val rand = new Random(System.currentTimeMillis())
  /*
   * The second argument means the source ID initializing random walk.
   */
  def apply(frdsMap: Map[Int, ArrayBuffer[Int]], backBone: scala.collection.Set[Int])(src: Int) = {
  
    val localGraph = new HashSet[Int]()
    var source = src
    localGraph += source
    var count=0;
    
    // when retry count is not more than 10,000, or backBone list doesn contain source
    // randomwalk continues.
    while(count<10000 && !backBone.contains(source)){

      val random_index = rand.nextInt(frdsMap(source).size)
      val random_node = frdsMap(source)(random_index)
      if(frdsMap.contains(random_node) && rand.nextDouble() <= math.min(1.0, frdsMap(random_node).size.toDouble/frdsMap(source).size.toDouble)) 
        source = random_node

      localGraph += source
      count = count + 1 
    }
    println("localGraph size = "+localGraph.size)
    localGraph
  }
}

object PageRankWalk{

  val rand = new Random(System.currentTimeMillis())
  /*
   * The second argument means the source ID initializing random walk.
   */
  def apply(frdsMap: Map[Int, ArrayBuffer[Int]], backBone: scala.collection.Set[Int])(src: Int) = {
  
    val conf = ConfigFactory.load
    val restartProb = conf.getDouble("restartProb")
    val localGraph = new HashSet[Int]()
    var source = src
    localGraph += source
    var count=0;
    
    // when retry count is not more than 10,000, or backBone list doesn contain source
    // randomwalk continues.
    while(count<10000 && !backBone.contains(source)){

      if(rand.nextDouble < restartProb) source = src
      val frds = frdsMap(source)
      var pairs = for(i<- 0 until frds.size) yield (i,frdsMap(frds(i)).size)
      val totalDegree = pairs.foldLeft(0)(( first:Int, elem:(Int, Int))=>{
        first + elem._2
      })
      val p2 = pairs.map{ case (first: Int, second: Int) =>(first, second.toDouble/totalDegree)}
      // die is a discrete prob distribution based on our algorithm
      val die = discrete(p2: _*)

      /*
       * die.sample is to sample a random variable from random distribution die.
       * 100 samples make the distribution stable.
       */
      source = frds(die.sample(100)(99))
      localGraph += source
      count += 1
    }
    println("localGraph size = "+localGraph.size)
    localGraph
  }
}
