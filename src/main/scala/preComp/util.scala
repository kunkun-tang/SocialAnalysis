package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.mutable.Map
import scala.collection.Set
import scala.util.control.Breaks._

object Util {

  val rand = new Random(System.currentTimeMillis());
  val conf = ConfigFactory.load

  var keyList: List[Int] = null;

  val SEARCH_MAX_LOOP = 1000;

  def genTwoSourceNodes(frdsMap: Map[Int, ArrayBuffer[Int]]) = {

    if(keyList == null)
      keyList = frdsMap.keySet.toList;

    val frdsMapSize  = frdsMap.size;
    
    val src1 = keyList(rand.nextInt(frdsMapSize));
    val src2 = keyList(rand.nextInt(frdsMapSize));
    (src1, src2)
  }

  /*
   * get two source nodes from MongoDB.
   */
  def genTwoSrcFromDB(dataSetName: String) = {

    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", conf.getInt("MongoDBPort"))
    val db = mongoClient(dataSetName+ "Split")
    val coll = db("test");

    var src1 = 0;
    var src2 = 0;
    val docuSize = coll.find().size;
    val doc1 = coll.find().limit(-1).skip(rand.nextInt(docuSize)).next();
    val doc2 = coll.find().limit(-1).skip(rand.nextInt(docuSize)).next();

    src1 = doc1.get("_id").toString.toInt;
    src2 = doc2.get("_id").toString.toInt;

    (src1, src2)
  }

  /*
   * get two source nodes from MongoDB.
   */
  def genTwoKnownSrcFromDB(dataSetName: String) = {

    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", conf.getInt("MongoDBPort"))
    val db = mongoClient(dataSetName+ "Split")
    val coll = db("test");

    val docuSize = coll.find().size;
    val doc1 = coll.find().limit(-1).skip(rand.nextInt(docuSize)).next();

    val k = doc1.toList(1)._1;
    val v = doc1.as[MongoDBList](k).toList; 
    val aFunction = new PartialFunction[Any, Int] {
      def apply(d: Any) = d match{
        case a: Int => a
      }
      def isDefinedAt(d: Any) = d match{
        case a: Int => true
        case _ => false
      }
    }

    val src1FrdList = v.collect(aFunction).to[ArrayBuffer];

    val src2 = src1FrdList(rand.nextInt(src1FrdList.length));

    (k.toInt, src2)
  }

  /*
   * get two source nodes from MongoDB.
   */
  def genFrdsMapFromDB(dataSetName: String) = {

    val frdsMap = scala.collection.mutable.Map[Int, ArrayBuffer[Int]]()

    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", conf.getInt("MongoDBPort"))
    val db = mongoClient(dataSetName+"Split")
    val coll = db("train");

    val cursor = coll.find();
    while(cursor.hasNext){
      val kv = cursor.next();
      val k = kv.toList(1)._1;
      val v = kv.as[MongoDBList](k).toList; 

      val aFunction = new PartialFunction[Any, Int] {
        def apply(d: Any) = d match{
          case a: Int => a
        }
        def isDefinedAt(d: Any) = d match{
          case a: Int => true
          case _ => false
        }
      }

      val newFrds = v.collect(aFunction).to[ArrayBuffer];

      frdsMap(k.toInt) = newFrds;
    }
    frdsMap
  }

  def findNumMutualFrds(frds1: ArrayBuffer[Int], frds2: ArrayBuffer[Int]) = {
    frds1.toSet.intersect(frds2.toSet).size
  }

  def findNumMutualComms(frds1: ArrayBuffer[Int], frds2: ArrayBuffer[Int]) = {
    if(frds1 == null || frds2 == null) 0
    else{
      frds1.toSet.intersect(frds2.toSet).size
    }
  }

  def sampleUniformQueryNodes(n: Int, frdsMap: Map[Int, ArrayBuffer[Int]], commMap: Map[Int, ArrayBuffer[Int]])={
    
    if(keyList == null)
      keyList = frdsMap.keySet.toList;

    /*
     * The sampling procedure works as follows:
     1). always find two random nodes, and get the mutual frds number of them two.
     2). append the pair to the value of Map if the limit is reached
     */
    def samplePairIterate(func: (ArrayBuffer[Int], ArrayBuffer[Int])=>Int, 
                          pairs: Map[Int, (Int, Int)]): Unit =  { 
      while(true) { 
      val a = keyList(rand.nextInt(keyList.size));
      val b = keyList(rand.nextInt(keyList.size));
      if(a<b){
        val num = func(frdsMap(a), frdsMap(b))
        if(num > 0 && num<=n && pairs.contains(num) == false){
          pairs += num->(a,b);
        }
      }
      if(pairs.size >= n) return
    }}

    /*
     * [mutualFrdsNum, Array*:([a1, a2], [b1, b2], ...)  ]
     * [a1, a2] is a nodes pair, which consists of two persons a1 and a2.
     */
    val samplePair = Map[Int, (Int, Int)]();
    samplePairIterate(findNumMutualFrds, samplePair);
    /*
     * [mutualCommsNum, Array*:([a1, a2], [b1, b2], ...)  ]
     */
    val commPair = Map[Int, (Int, Int)]();
    samplePairIterate(findNumMutualComms, commPair);

    // println(samplePair);
    (samplePair, commPair)
  }

  import scala.collection.mutable.HashSet
  def prune(aMap: Map[Int, ArrayBuffer[Int]], keepSet: HashSet[Int]) = {

    /*
     * keepSet collects nodes id which is remained in the fiveSet, and we should keep them
     * in the final frdsMap. Then, we filter out all nodes which is not in fiveSet.
     */
    var frdsMap = aMap.filter{ case (k,v) => keepSet.contains(k) == true};
    for( (k,v)<-frdsMap) {
      val replaceV = v.filter{ case elem:Int => keepSet.contains(elem) == true};
      frdsMap(k) = replaceV;
    }
    frdsMap
  }
  
}
