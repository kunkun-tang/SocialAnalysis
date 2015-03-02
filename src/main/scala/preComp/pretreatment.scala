package preComp

import com.typesafe.config.ConfigFactory
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

object PreMain {

  /*
   * frdsMap is (userID, ArrayBuffer(userID_63, userID_22, ...)) 
        ArrayBuffer is userID's friends list.
   * commsMap is (userID, ArrayBuffer(commID_23, commID_45, ...)) 
        ArrayBuffer is userID's community list.
   */
  val frdsMap = scala.collection.mutable.Map[Int, ArrayBuffer[Int]]()
  val commsMap = scala.collection.mutable.Map[Int, ArrayBuffer[Int]]()

  def putFrds(tuple: Array[String])= if(tuple.length == 2){
    val frd1 = tuple(0).toInt
    val frd2 = tuple(1).toInt
    if(frdsMap(frd1) == null) frdsMap(frd1) = ArrayBuffer[Int]()
    if(frdsMap(frd2) == null) frdsMap(frd2) = ArrayBuffer[Int]()
    frdsMap(frd1) += frd2
    frdsMap(frd2) += frd1
  }

  def putComms(tuple: Array[String], commID: Int)= {
    for(i<- 0 until tuple.length){
      val num = tuple(i).toInt;
      if(commsMap(num) == null) commsMap(num) = new ArrayBuffer[Int]();
      commsMap(num) += commID;
    }
  }

  /*
   * pruneFrds makes users whose degree is always less than a threshold,
   * delete related info in commMap meanwhile.
   */
  def pruneFrds(aMap: scala.collection.mutable.Map[Int, ArrayBuffer[Int]], 
                commMap: scala.collection.mutable.Map[Int, ArrayBuffer[Int]], 
                smallDegree: Int)={

    val smallDegreeNodes = new HashSet[Int]();
    for( (k,v)<-aMap; if(v==null || v.size < smallDegree)) {
      aMap -= k;
      commMap -= k;
      smallDegreeNodes += k;
    }
    println("first prune frdsMap size = " + aMap.size)

    /*
     * smallDegreeNodes collects nodes whose degree is less than threshold, and we filter all 
     * these nodes in other nodes' friends.
     */
    for( (k,v)<-aMap) {
      val replace = v.filter{ case elem:Int => smallDegreeNodes.contains(elem) == false};
      if(replace.size> 0)
        aMap(k) = replace;
      else aMap -= k;
    }  
  }

  def applyMemory(dataSetName: String) = {
    val conf = ConfigFactory.load

    /*
     * init the frdsMap by assigning every Int with a null Friends List
     */
    for(i<- 0 to conf.getInt(dataSetName+".maxIDs")){
      frdsMap(i) = null
      commsMap(i) = null
    }

    /**
     * file is a file for DBLP data, but a folder for LJ data.
     * we operate DBLP data directly, but
     * Because livejournal dataset is too large, we split it into several medium-size files.
     */
    import org.apache.commons.io.LineIterator;
    import org.apache.commons.io.FileUtils;
    val file = new java.io.File(conf.getString(dataSetName+".friendsFile"))
    if(!file.isDirectory()){
      val it = FileUtils.lineIterator(file, "UTF-8");
      try {
          while (it.hasNext()) {
              val line = it.nextLine();
              putFrds(line.split('\t'))
          }
      } finally {
          LineIterator.closeQuietly(it);
      }
    }
    else{
        for( file <- file.listFiles(); if(file.getName().charAt(0)=='x')){
          val it = FileUtils.lineIterator(file, "UTF-8");
          try {
              while (it.hasNext()) {
                  val line = it.nextLine();
                  putFrds(line.split('\t'))
              }
          } finally {
              LineIterator.closeQuietly(it);
          }
          println(file.getName() + " done");
        }
    }


    var commID = 0;
    val commFile = new java.io.File(conf.getString(dataSetName+".communityFile"))
    val itFile = FileUtils.lineIterator(commFile, "UTF-8");
    try {
        while (itFile.hasNext()) {
          val line = itFile.nextLine();
          putComms(line.split('\t'), commID);
          commID += 1;
        }
    } finally {
        LineIterator.closeQuietly(itFile);
    }

    println("after reading files.")

    /*
     * prune people whose degree is less than a threshold
     */
    pruneFrds(frdsMap, commsMap, conf.getInt(dataSetName+".filterSmallDegree"))
    println("after prune commsMap size = " + commsMap.size)
    println("after prune frdsMap size = " + frdsMap.size)

    /*
     * compute backBoneGraph with configuration.
     */
    val backBone= frdsMap.filter{ case (k,v)=> v.size>conf.getInt(dataSetName+".BackBoneDegree")}.keySet
    println("backBone size = " + backBone.size)
    (frdsMap, commsMap, backBone)
  }


  def applySemiDB(dataSetName: String) = {
    val conf = ConfigFactory.load

    /*
     * init the frdsMap by assigning every Int with a null Friends List
     */
    for(i<- 0 to conf.getInt(dataSetName+".maxIDs")){
      frdsMap(i) = null
      commsMap(i) = null
    }

    /**
     * file is a file for DBLP data, but a folder for LJ data.
     * we operate DBLP data directly, but
     * Because livejournal dataset is too large, we split it into several medium-size files.
     */
    import org.apache.commons.io.LineIterator;
    import org.apache.commons.io.FileUtils;
    val file = new java.io.File(conf.getString(dataSetName+".friendsFile"))
    if(!file.isDirectory()){
      val it = FileUtils.lineIterator(file, "UTF-8");
      try {
          while (it.hasNext()) {
              val line = it.nextLine();
              putFrds(line.split('\t'))
          }
      } finally {
          LineIterator.closeQuietly(it);
      }
    }
    else{
        for( file <- file.listFiles(); if(file.getName().charAt(0)=='x')){
          val it = FileUtils.lineIterator(file, "UTF-8");
          try {
              while (it.hasNext()) {
                  val line = it.nextLine();
                  putFrds(line.split('\t'))
              }
          } finally {
              LineIterator.closeQuietly(it);
          }
          println(file.getName() + " done");
        }
    }


    var commID = 0;
    val commFile = new java.io.File(conf.getString(dataSetName+".communityFile"))
    val itFile = FileUtils.lineIterator(commFile, "UTF-8");
    try {
        while (itFile.hasNext()) {
          val line = itFile.nextLine();
          putComms(line.split('\t'), commID);
          commID += 1;
        }
    } finally {
        LineIterator.closeQuietly(itFile);
    }

    println("after reading files.")

    /*
     * prune people whose degree is less than a threshold
     */
    pruneFrds(frdsMap, commsMap, conf.getInt(dataSetName+".filterSmallDegree"))

    println("after prune commsMap size = " + commsMap.size)
    println("after prune frdsMap size = " + frdsMap.size)

    /*
     * compute backBoneGraph with configuration.
     */
    val backBone= frdsMap.filter{ case (k,v)=> v.size>conf.getInt(dataSetName+".BackBoneDegree")}.keySet
    println("backBone size = " + backBone.size)

    /*
     * insert frdsMap to MongoDB.
     */
    import com.mongodb.casbah.Imports._
    val mongoClient = MongoClient(conf.getString("MongoDBHost"), conf.getInt("MongoDBPort"))
    val db = mongoClient("liang")

    val coll = db("liang")
    // coll.drop();

    for((k,v) <- frdsMap){
      val list = MongoDBList();
      v.foreach(e=>{list += e});
      coll.insert(MongoDBObject("_id" -> k, k.toString -> list))
    }
    (frdsMap, commsMap, backBone)
  }



  def applyDB(dataSetName: String) = {
    val conf = ConfigFactory.load
    /*
     * init the frdsMap by assigning every Int with a null Friends List
     */
    for(i<- 0 to conf.getInt(dataSetName+".maxIDs")){
      commsMap(i) = null
    }

    /**
     * file is a file for DBLP data, but a folder for LJ data.
     * we operate DBLP data directly, but
     * Because livejournal dataset is too large, we split it into several medium-size files.
     */
    import org.apache.commons.io.LineIterator;
    import org.apache.commons.io.FileUtils;
    import com.mongodb.casbah.Imports._
    import javax.net.SocketFactory;

    import com.mongodb.casbah.MongoClientOptions._;


    // val Socket = new SocketFactory()

    val options = new Builder().connectionsPerHost(10).build();

    val mongoClient = MongoClient(conf.getString("MongoDBHost"), conf.getInt("MongoDBPort"))

    val db = mongoClient(dataSetName+"Split")
    val coll = db("train");
    // coll.drop();

    if(conf.getBoolean(dataSetName + ".writeDB")){
      var previous = -1;
      var count = 0;
      var dynamicFrds = new ArrayBuffer[Int]();

      def putMONGOFrds(tuple: Array[String])= if(tuple.length == 2){
        val frd1 = tuple(0).toInt;
        val frd2 = tuple(1).toInt;
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

    var commID = 0;
    val commFile = new java.io.File(conf.getString(dataSetName+".communityFile"))
    val itFile = FileUtils.lineIterator(commFile, "UTF-8");
    try {
      while (itFile.hasNext()) {
        val line = itFile.nextLine();
        putComms(line.split('\t'), commID);
        commID += 1;
      }
    } finally {
      LineIterator.closeQuietly(itFile);
    }

    println("after reading files.")


    if(conf.getBoolean(dataSetName + ".pruneData")){
      /*
       * pruneLJFrds makes users whose degree is always less than a threshold,
       * delete related info in commMap meanwhile.
       */
      def pruneLJFrds(commMap: scala.collection.mutable.Map[Int, ArrayBuffer[Int]], 
                    smallDegree: Int)={

        val smallDegreeNodes = new HashSet[Int]();

        var cursor = coll.find();
        while(cursor.hasNext){
          val kv1 = cursor.next();
          val k1 = kv1.toList(1)._1;
          val v1 = kv1.as[MongoDBList](k1).toList; 
          if(v1.size < smallDegree){
            commMap -= k1.toInt;
            smallDegreeNodes += k1.toInt;
            coll.remove(kv1);
          }
        }

        /*
         * smallDegreeNodes collects nodes whose degree is less than threshold, and we filter all 
         * these nodes in other nodes' friends. For exmaple(5-> [1, 2, 3, 6, 7]), because node 6 is in small
         * degree nodes set, then we should delete node 6 in 5's friends. Then it should be 5-> [1, 2, 3, 7]
         */
        cursor = coll.find();
        println("after first prune cursor size = "+ cursor.size);
        while(cursor.hasNext){
          val kv = cursor.next();
          val k = kv.toList(1)._1;
          val v = kv.as[MongoDBList](k).toList; 
          val friends = v.filter{_.isInstanceOf[Int]}

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
          /*
           * cast list to mongodblist.
           */
          val dbList = MongoDBList(newFrds:_*)
          val query = MongoDBObject("_id" -> k.toInt)
          val update = MongoDBObject(k.toString -> dbList)

          if(newFrds.size > 0)  coll.update(query, update)
          else  coll.remove(query);
        }
      }

      /*
       * prune people whose degree is less than a threshold
       */
      println("before prune frdsMap size = " + coll.find().size)
      pruneLJFrds(commsMap, conf.getInt(dataSetName+".filterSmallDegree"))
    }

    /*
     * compute backBoneGraph with configuration.
     */
    // var cursor = coll.find();
    // println("after prune commsMap size = " + commsMap.size)
    println("after prune frdsMap size = " + coll.find().size)

    val backBone = coll.find().flatMap{ 
      case p: DBObject => {
          val key = p.toList(1)._1;
          val v = p.as[MongoDBList](key).toList; 
          if(v.size > conf.getInt(dataSetName+".BackBoneDegree")) Some(key.toInt)
          else None
    }}.toSet

    println("backBone size = " + backBone.size)

    (frdsMap, commsMap, backBone)
  }

}
