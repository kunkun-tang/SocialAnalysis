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
    /*
     * smallDegreeNodes collects nodes whose degree is less than threshold, and we filter all 
     * these nodes in other nodes' friends.
     */
    for( (k,v)<-aMap) {
      val replace = v.filter{ case elem:Int => smallDegreeNodes.contains(elem) == false};
      aMap(k) = replace;
    }  
  }


  def apply(dataSetName: String) = {
    val conf = ConfigFactory.load

    /*
     * init the frdsMap by assigning every Int with a null Friends List
     */
    for(i<- 0 to conf.getInt(dataSetName+".maxIDs")){
      frdsMap(i) = null
      commsMap(i) = null
    }

    // println("before reading files.")
    // Source.fromFile(conf.getString(dataSetName+".friendsFile")).getLines.
    // foreach{ 
    //   line => putFrds(line.split('\t'))
    // }

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
    val mongoClient = MongoClient("localhost", 27017)
    val db = mongoClient("liang")

    val coll = db("liang")
    coll.drop();

    for((k,v) <- frdsMap){
      val list = MongoDBList();
      v.foreach(e=>{list += e});
      coll.insert(MongoDBObject(k.toString -> list))
    }

    (frdsMap, commsMap, backBone)
  }



  def apply() = {
    val conf = ConfigFactory.load

    val dataSetName = "LiveJournal";
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
    val mongoClient = MongoClient("localhost", 27017)
    val db = mongoClient("liang")
    val coll = db("liang")


    val list = MongoDBList();
    v.foreach(e=>{list += e});
    coll.insert(MongoDBObject(k.toString -> list))

      
    val file = new java.io.File(conf.getString(dataSetName+".friendsFile"))
    val it = FileUtils.lineIterator(file, "UTF-8");
    try {
        while (it.hasNext()) {
            val line = it.nextLine();
            putFrds(line.split('\t'))
        }
    } finally {
        LineIterator.closeQuietly(it);
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
    val mongoClient = MongoClient("localhost", 27017)
    val db = mongoClient("liang")

    val coll = db("liang")
    coll.drop();

    for((k,v) <- frdsMap){
      val list = MongoDBList();
      v.foreach(e=>{list += e});
      coll.insert(MongoDBObject(k.toString -> list))
    }

    (frdsMap, commsMap, backBone)
  }

}
