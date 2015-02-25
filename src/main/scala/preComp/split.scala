package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.util.Random

object Split {

  val rand = new Random(System.currentTimeMillis())
  def applyDB(dataSetName: String) = {
    val conf = ConfigFactory.load

    /**
     * file is a file for DBLP data, but a folder for LJ data.
     * we operate DBLP data directly, but
     * Because livejournal dataset is too large, we split it into several medium-size files.
     */
    import org.apache.commons.io.LineIterator;
    import org.apache.commons.io.FileUtils;
    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient("localhost", conf.getInt("MongoDBPort"))

    val db = mongoClient(dataSetName+"Split")
    val coll = db("train");
    val collTest = db("test");


    def putOnColl(frd1: Int, frd2: Int, coll: MongoCollection) = {

      /*
       * Based on (p, q) pair, put friend q to p's friends List.
       * Similarly, put p to q's friends List.
       */
      val query1 =  MongoDBObject("_id" -> frd1);
      var cursor = coll.find(query1);
      if(cursor.hasNext){
        val insert = MongoDBObject("$push" -> MongoDBObject(frd1.toString -> frd2))
        coll.update(query1, insert);
      }
      else{
        val insert = MongoDBObject("_id" -> frd1, frd1.toString -> MongoDBList(frd2));
        coll.insert(insert);
      }

      val query2 =  MongoDBObject("_id" -> frd2);
      cursor = coll.find(query2);
      if(cursor.hasNext){
        val insert = MongoDBObject("$push" -> MongoDBObject(frd2.toString -> frd1))
        coll.update(query2, insert);
      }
      else{
        val insert = MongoDBObject("_id" -> frd2, frd2.toString -> MongoDBList(frd1));
        coll.insert(insert);
      }
    }

    if(conf.getBoolean(dataSetName + ".writeDB")){
      var previous = -1;
      var count = 0;
      var dynamicFrds = new ArrayBuffer[Int]();

      def putMONGOFrds(tuple: Array[String])= if(tuple.length == 2){
        val frd1 = tuple(0).toInt;
        val frd2 = tuple(1).toInt;

        if(rand.nextDouble()<0.9) putOnColl(frd1, frd2, coll);
        else putOnColl(frd1, frd2, collTest);
      }

      val file = new java.io.File(conf.getString(dataSetName+".friendsFile"))
      val it = FileUtils.lineIterator(file, "UTF-8");
      try {
        while (it.hasNext()){
          val line = it.nextLine();
          putMONGOFrds(line.split('\t'));
          count += 1;
          if(count %10000 == 0) println("line number: " + count);
        }
      } finally {
          LineIterator.closeQuietly(it);
      }
    }

    println("after reading files.")

  }

  def main(args: Array[String]) = {
    Split.applyDB("DBLP")
  }
}
