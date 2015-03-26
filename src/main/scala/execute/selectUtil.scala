package execute

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.mutable.Map
import scala.collection.Set
import scala.util.control.Breaks._
import preComp._

object SelectUtilKnown {

  import Util._;
  val conf = ConfigFactory.load
  val datasetName = conf.getString("DataSetName");
  val rand = new Random(System.currentTimeMillis())

  import com.mongodb.casbah.Imports._
  val mongoClient = MongoClient(conf.getString("MongoDBHost"), conf.getInt("MongoDBPort"))
  val db = mongoClient(datasetName+ "Split")
  val collTest = db("test");

	def main(args: Array[String])={

	  var (frdsMap, commsMap, backBone) = PreMain.applyDB(datasetName)
  	frdsMap = Util.genFrdsMapFromDB(datasetName)

	  val commBigDegreeUsers = commsMap.toList.filter{ case (k,v)=> if(v== null) false
	  	else v.length > 3}.map(_._1)

	  val result = for(
	  	user1 <- commBigDegreeUsers;
	  	user2 <- commBigDegreeUsers if(user1 != user2)
	  	if (rand.nextDouble()<0.7 
	  		&& Util.findNumMutualFrds(user1, user2, frdsMap) > 3 
	  		&& Util.findNumMutualComms(user1, user2, commsMap) > 3 
	  		&& knowEachOther(user1, user2, frdsMap) ==true)
	  )yield(user1, user2)

	  // val result = for(
	  // 	user1 <- commBigDegreeUsers;
	  // 	user2 <- commBigDegreeUsers if(user1 != user2)
	  // 	if (rand.nextDouble()<0.7 && Util.findNumMutualFrds(user1, user2, frdsMap) > 3 && Util.findNumMutualComms(user1, user2, commsMap) > 3 && knowEachOther(user1, user2, frdsMap) ==true)
	  // )println( Util.findNumMutualFrds(user1, user2, frdsMap) + " " + Util.findNumMutualComms(user1, user2, commsMap) )


	  println("complete")

	  printToFile(new java.io.File("selectKnown_1_1.txt"))(p => {
	    result.foreach(res => p.println(res._1 + " " + res._2))
	  })
	}

	def knowEachOther(user1: Int, user2: Int, frdsMap: scala.collection.mutable.Map[Int, ArrayBuffer[Int]]) = {

    val query = MongoDBObject("_id" -> user1)
    val cursor = collTest.find(query);
    var ret = false;
    val aFunction = new PartialFunction[Any, Int] {
      def apply(d: Any) = d match{
        case a: Int => a
      }
      def isDefinedAt(d: Any) = d match{
        case a: Int => true
        case _ => false
      }
    }
    if(cursor.hasNext){
	    val kv = cursor.next();
      
      val k1 = kv.toList(1)._1;
      val v1 = kv.as[MongoDBList](k1).toList; 
      val src1FrdListInTest = v1.collect(aFunction).to[ArrayBuffer];
      if(src1FrdListInTest.contains(user2) == true) ret = true;
    }

    ret;
	}

	def readSelectFile(fileName: String)={
    Source.fromFile(fileName).getLines.map( str => (str.split(" ")(0).toInt, str.split(" ")(1).toInt)).toList
	}

}


object SelectUtilStranger {

  import Util._;
  val conf = ConfigFactory.load
  val datasetName = conf.getString("DataSetName");
  val rand = new Random(System.currentTimeMillis())

	def main(args: Array[String])={

	  var (frdsMap, commsMap, backBone) = PreMain.applyDB(datasetName)
  	frdsMap = Util.genFrdsMapFromDB(datasetName)

	  val commBigDegreeUsers = commsMap.toList.filter{ case (k,v)=> if(v== null) false
	  	else v.length > 3}.map(_._1)

	  println(commBigDegreeUsers.size)

	  val result = for(
	  	user1 <- commBigDegreeUsers;
	  	user2 <- commBigDegreeUsers if(user1 != user2)
	  	if (rand.nextDouble()<0.7 
	  		&& Util.findNumMutualFrds(user1, user2, frdsMap) > 3 
	  		&& Util.findNumMutualComms(user1, user2, commsMap) > 3 
	  		&& knowEachOther(user1, user2, frdsMap) ==false)
	  )yield(user1, user2)

	  val result = for(
	  	user1 <- commBigDegreeUsers;
	  	user2 <- commBigDegreeUsers if(user1 != user2)
	  	if (rand.nextDouble()<0.7 
	  		&& Util.findNumMutualFrds(user1, user2, frdsMap) > 3 
	  		&& Util.findNumMutualComms(user1, user2, commsMap) > 3 
	  		&& knowEachOther(user1, user2, frdsMap) ==false)
	  )println(user1 + " " + user2)

	  println("complete")

	  printToFile(new java.io.File("selectStranger.txt"))(p => {
	    result.foreach(res => p.println(res._1 + " " + res._2))
	  })
	}

	def knowEachOther(user1: Int, user2: Int, frdsMap: scala.collection.mutable.Map[Int, ArrayBuffer[Int]]) = {
		val v1 = frdsMap(user1);
		v1.contains(user2)
	}

	def readSelectFile(fileName: String)={
    Source.fromFile(fileName).getLines.map( str => (str.split(" ")(0).toInt, str.split(" ")(1).toInt)).toList
	}

}