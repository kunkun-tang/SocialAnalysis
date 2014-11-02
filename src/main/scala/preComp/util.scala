package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.Set

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
    val db = mongoClient(dataSetName)
    val coll = db("liang");

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
  def genFrdsMapFromDB(dataSetName: String) = {

    val frdsMap = scala.collection.mutable.Map[Int, ArrayBuffer[Int]]()

    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", conf.getInt("MongoDBPort"))
    val db = mongoClient(dataSetName)
    val coll = db("liang");

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

}
