package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import com.mongodb.casbah.Imports._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

object DB{


class DBObjectHelper(underlying: DBObject) {

  def asString(key: String) = underlying.as[String](key)

  def asDouble(key: String) = underlying.as[Double](key)

  def asInt(key: String) = underlying.as[Int](key)

  def asList[A](key: String) =
    (List() ++ underlying(key).asInstanceOf[BasicDBList]) map { _.asInstanceOf[A] }

  def asDoubleList(key: String) = asList[Double](key)
}

object DBObjectHelper {

  implicit def toDBObjectHelper(obj: DBObject) = new DBObjectHelper(obj)

}

	val mongoClient = MongoClient("localhost", 27017)
	val db = mongoClient("test")

	def apply() ={
		println(db.collectionNames)
		val coll = db("liang")
		coll.drop();
		
		val l1 = MongoDBList(2, 3, 4)
		val a1 = MongoDBObject("_id" -> 1, "1" -> l1)
		coll.insert(a1)

		val l2 = MongoDBList(3, 4)
		val a2 = MongoDBObject("_id" -> 2, "2" -> l2)
		coll.insert(a2)

		val l3 = MongoDBList(4, 5)
		val a3 = MongoDBObject("_id" -> 3, "3" -> l3)
		coll.insert(a3)

		val l4 = MongoDBList(5)
		val a4 = MongoDBObject("_id" -> 4, "4" -> l4)
		coll.insert(a4)

    val l5 = MongoDBList(6)
    val a5 = MongoDBObject("_id" -> 5, "5" -> l5)
    coll.insert(a5)

    // val query =  MongoDBObject("_id" -> 4);
    // val cursor = coll.find(query);
    // if(cursor.hasNext)
    //   coll.aggreate();

    // for(kv<- coll.find()){
    //     val k1 = kv.toList(1)._1;
    //     val v1 = kv.as[MongoDBList](k1).toList; 
    //     val friends = v1.filter{_.isInstanceOf[Int]}
    //     for(i <- friends){
    //       val query =  MongoDBObject("_id" -> i);
    //       val cursor = coll.find(query);
    //       if(cursor.hasNext){
    //         val obj = cursor.next();
    //         val k2 = obj.toList(1)._1;
    //         var currList = obj.as[MongoDBList](k2).toList;
    //         val list2 = currList ::: List(k1.toInt); 
    //         val insert = MongoDBObject(i.toString -> MongoDBList(list2:_*))
    //         coll.update(query, insert);
    //       }
    //       else{
    //         val insert = MongoDBObject("_id" -> i, i.toString -> MongoDBList(k1.toInt))
    //         coll.insert(insert);
    //       }
    //     }
    // }


		// var cursor = coll.find();
		// val v1 = a.getAs[MongoDBList]("hello"); 
		// v1.get.collect{ case i: Int => println(i)}
		// println(v1.get)

	 //  cursor = coll.find();
	 //  val smallDegreeNodes = new HashSet[Int]();

	 //  println(cursor.size);
  //   while(cursor.hasNext){
  //     val kv1 = cursor.next();
  //     val k1 = kv1.toList(1)._1;
  //     val v1 = kv1.as[MongoDBList](k1).toList;
  //     println(kv1._id+ " " + k1 + " "+ v1)
  //     if(k1.toString.equals("123")){
  //     	smallDegreeNodes += k1.toInt
  //     	coll.remove(kv1);
  //     }

  	// 	val query = MongoDBObject("_id" -> kv1._id.get)
  	// 	val update = MongoDBObject(k1.toString -> "JVM")
  	// 	val result = coll.update( query, update )

  //   }
    // println(smallDegreeNodes)

	  // cursor = coll.find();
	  // println(cursor.size);
	  
//	  pruneLJFrds(2)
    def pruneLJFrds(smallDegree: Int)={

      val smallDegreeNodes = new HashSet[Int]();

      var cursor = coll.find();
      while(cursor.hasNext){
        val kv1 = cursor.next();
        val k1 = kv1.toList(1)._1;
        val v1 = kv1.as[MongoDBList](k1).toList; 
        // val v1 = kv1.toList(1)._2;
        // v1 match{
        //   case elem: BasicDBList =>println(v1 + " "+ v1.getClass)
        //   case _ => println("other")
        // }
        if(v1.size < smallDegree){
          smallDegreeNodes += k1.toInt;
          coll.remove(kv1);
        }
      }
      /*
       * smallDegreeNodes collects nodes whose degree is less than threshold, and we filter all 
       * these nodes in other nodes' friends.
       */
      cursor = coll.find();
      while(cursor.hasNext){
        val kv = cursor.next();
        val k = kv.toList(1)._1;
        val v = kv.as[MongoDBList](k).toList; 
        val friends = v.filter{_.isInstanceOf[Int]}
        // val newFrds = new ArrayBuffer[Int]()
        println(friends);
        val aFunction = new PartialFunction[Any, Int] {
          def apply(d: Any) = d match{
            case a: Int => a
          }
          def isDefinedAt(d: Any) = d match{
            case a: Int => true
            case _ => false
          }
        }

        val newFrds = friends.collect(aFunction).filter{case elem: Int=> 
          smallDegreeNodes.contains(elem) == false};
        println(newFrds);

        val dbList = MongoDBList(newFrds:_*)
        
        val query = MongoDBObject("_id" -> kv._id.get)
        val update = MongoDBObject(k.toString -> dbList)
        if(newFrds.size > 0)
          coll.update(query, update)
        else
          coll.remove(query);

      }
    }


    // val query =  MongoDBObject("_id" -> MongoDBObject("$in" -> MongoDBList(1,2,3)));
    // coll.remove(query);


    // val results = coll.find().flatMap{ 
    //   case p: DBObject => {
    //                         val key = p.toList(1)._1;
    //                         val v = p.as[MongoDBList](key).toList; 
    //                         if(v.size > 1) Some(key)
    //                         else None
    //                       }

    // }

    // println(results.toList)

    // for(p <- coll.find(); if(p.toList(1)._2.size > 1)) yield p.toList(1)._1;
    // val results = coll.find().foreach{
    //   case p: DBObject=>if(p.as[MongoDBList](k).size > 1) yield p.toList(1)._1;
    // }

		// val in = MongoDBObject("hello" -> "aasdfasdf")
		// coll.update(MongoDBObject(), in)


		// import scala.collection.mutable.HashMap;
		// val aMap = HashMap[Int, ArrayBuffer[Int]]();
		// aMap += 1->ArrayBuffer(2,3,4,5);
		// aMap += 2->ArrayBuffer(3,4,5,6,7);
  //   for( (k,v)<-aMap) {
  //     val replace = v.filter{ case elem:Int => elem>3};
  //     aMap(k) = replace;
  //   }
  //   println(aMap)  



  }




  def main(args: Array[String]){
    DB();
  }
}
