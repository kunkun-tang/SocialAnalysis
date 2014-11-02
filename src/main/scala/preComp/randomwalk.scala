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
  val conf = ConfigFactory.load

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
//      print(source+" ")
      val random_index = rand.nextInt(frdsMap(source).size)
      val random_node = frdsMap(source)(random_index)
      if(frdsMap.contains(random_node) && rand.nextDouble() <= math.min(1.0, frdsMap(random_node).size.toDouble/frdsMap(source).size.toDouble)) 
        source = random_node

      localGraph += source
      count = count + 1
    }
//    println(" ")
    localGraph
  }

  def applyDB(backBone: scala.collection.Set[Int])(src: Int, dataSetName: String) = {
  
    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", conf.getInt("MongoDBPort"))
    val db = mongoClient(dataSetName)
    val coll = db("liang");

    val localGraph = new HashSet[Int]()
    var source = src
    localGraph += source
    var count=0;

    localGraph
  }

}

object PageRankWalk{

  val rand = new Random(System.currentTimeMillis())
  val conf = ConfigFactory.load

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
//      print(source+" ")

      val frds = frdsMap(source)
      var pairs = for(i<- 0 until frds.size; if(frdsMap.contains(frds(i)))) 
        yield (i, frdsMap(frds(i)).size)
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

      val newNode = frds(die.sample(100)(99))
      if(frdsMap.contains(newNode))
        source = newNode;

      localGraph += source
      count += 1
    }
//    println(" ")
    if(count == 10000) println("too many loops in PAGERANK random walk.")
    localGraph
  }
}
