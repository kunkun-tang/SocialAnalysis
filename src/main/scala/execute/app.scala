package execute

import com.typesafe.config.ConfigFactory
import preComp._
import infer._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

/*
 * SocialAnalysis Main function.
 */
object PAGERANKApp extends App{

  val conf = ConfigFactory.load
  val (frdsMap, commsMap, backBone) = PreMain.applyMemory("DBLP")

  val TEST_SOURCE_NUM = conf.getInt("DBLP.TEST_SOURCE_NUM")

  // var src1 = conf.getInt("DBLP.src1")
  // var src2 = conf.getInt("DBLP.src2");

  var count = 0;
  var LOCALSET_INCREMENT = 0;

	println("localSet size = "+LOCALSET_INCREMENT/TEST_SOURCE_NUM + "  backBone set = "+ backBone.size)

}

object MetroPolisApp extends App{

  val conf = ConfigFactory.load
  val (frdsMap, commsMap, backBone) = PreMain.applyMemory("DBLP")

  val TEST_SOURCE_NUM = conf.getInt("DBLP.TEST_SOURCE_NUM")

  // var src1 = conf.getInt("DBLP.src1")
  // var src2 = conf.getInt("DBLP.src2");

  var count = 0;
  var LOCALSET_INCREMENT = 0;
  while(count < TEST_SOURCE_NUM){

	  val (src1, src2) = Util.genTwoSrcFromDB("DBLP");

	  val localGraph = RWM(frdsMap, backBone)(src1)
	  val localGraph2 = RWM(frdsMap, backBone)(src2)

	  val localSet = localGraph ++ localGraph2 
	  LOCALSET_INCREMENT += localSet.size
	  count += 1;
	}

	 println("localSet size = "+LOCALSET_INCREMENT/TEST_SOURCE_NUM + "  backBone set = "+ backBone.size)

}

object MetroPolisDBApp extends App{

  val conf = ConfigFactory.load
  val datasetName = "LiveJournal";
  val (frdsMap_0, commsMap, backBone) = PreMain.applyDB(datasetName)

  val TEST_SOURCE_NUM = conf.getInt("LiveJournal.TEST_SOURCE_NUM")
  val frdsMap = Util.genFrdsMapFromDB(datasetName)

  var count = 0;
  var LOCALSET_INCREMENT = 0;
  while(count < TEST_SOURCE_NUM){

	  val (src1, src2) = Util.genTwoSourceNodes(frdsMap);

	  val localGraph = RWM.apply(frdsMap, backBone)(src1)
	  val localGraph2 = RWM.apply(frdsMap, backBone)(src2)

	  val localSet = localGraph ++ localGraph2 
	  LOCALSET_INCREMENT += localSet.size
	  count += 1;
	}

	 println("localSet size = "+LOCALSET_INCREMENT/TEST_SOURCE_NUM + "  backBone set = "+ backBone.size)

}

object PageRankDBApp extends App{

  val conf = ConfigFactory.load
  val datasetName = "LiveJournal";
  val (frdsMap_0, commsMap, backBone) = PreMain.applyDB(datasetName)

  val TEST_SOURCE_NUM = conf.getInt("LiveJournal.TEST_SOURCE_NUM")
  val frdsMap = Util.genFrdsMapFromDB(datasetName)

  var count = 0;
  var LOCALSET_INCREMENT = 0;
  while(count < TEST_SOURCE_NUM){

	  val (src1, src2) = Util.genTwoSourceNodes(frdsMap);

	  val localGraph = PageRankWalk.apply(frdsMap, backBone)(src1)
	  val localGraph2 = PageRankWalk.apply(frdsMap, backBone)(src2)

	  val localSet = localGraph ++ localGraph2 
	  LOCALSET_INCREMENT += localSet.size
	  count += 1;
	}

	 println("localSet size = "+LOCALSET_INCREMENT/TEST_SOURCE_NUM + "  backBone set = "+ backBone.size)
}


object InferApp extends App{

  val conf = ConfigFactory.load
  val datasetName = "DBLP";
  val (frdsMap, commsMap, backBone) = PreMain.applyMemory(datasetName)

  // val frdsMap = Util.genFrdsMapFromDB(datasetName)
  /*
   * frdsPair consist of sample pair of two nodes, which have mutual friend number information.
   * commsPair includes the mutual community number key value.
   */
  // val (frdsPair, commsPair) = Util.sampleUniformQueryNodes(2, frdsMap, commsMap);
  var (src1, src2) = Util.genTwoSourceNodes(frdsMap);
  if(src1 > src2){
    var temp = src1;
    src1 = src2
    src2 = temp;
  }

  val localGraph = RWM.apply(frdsMap, backBone)(src1)
  val localGraph2 = RWM.apply(frdsMap, backBone)(src2)
  val fiveSet = localGraph ++ localGraph2  ++ backBone

  
  // inferFrdsMap is the final fiveSet frdsMap.
   
  val inferFrdsMap = Util.prune(frdsMap, fiveSet);
  println("fiveSet size = " + inferFrdsMap.size)
  // }
  println("inferFrdsMap.contains(src1) = " + inferFrdsMap.contains(src1))
  println("inferFrdsMap.contains(src2) = " + inferFrdsMap.contains(src2))
  MCSAT(inferFrdsMap,commsMap)(src1,src2)
}

object DBLPInferApp extends App{

  def findNumMutualFrds(src1: Int, src2: Int, frdsMapGlobal: scala.collection.mutable.Map[Int, ArrayBuffer[Int]]) = {
    val frds1 = frdsMapGlobal(src1);
    val frds2 = frdsMapGlobal(src2);
    frds1.toSet.intersect(frds2.toSet).size
  }

  val conf = ConfigFactory.load
  val datasetName = conf.getString("DataSetName");
  var (frdsMap, commsMap, backBone) = PreMain.applyDB(datasetName)

  frdsMap = Util.genFrdsMapFromDB(datasetName)
  /*
   * frdsPair consist of sample pair of two nodes, which have mutual friend number information.
   * commsPair includes the mutual community number key value.
   */
  // val (frdsPair, commsPair) = Util.sampleUniformQueryNodes(2, frdsMap, commsMap);
  var (src1, src2) = Util.genTwoSrcFromDB(datasetName);
  if(src1 > src2){
    var temp = src1;
    src1 = src2
    src2 = temp;
  }
  val numMutualActualFrds = findNumMutualFrds(src1, src2, frdsMap);

  val localGraph = RWM.apply(frdsMap, backBone)(src1)
  val localGraph2 = RWM.apply(frdsMap, backBone)(src2)
  val fiveSet = localGraph ++ localGraph2  ++ backBone

  /*
   * inferFrdsMap is the final fiveSet frdsMap.
   */
  val inferFrdsMap = Util.prune(frdsMap, fiveSet);
  println("fiveSet size = " + inferFrdsMap.size)
  // }
  println("inferFrdsMap.contains(src1) = " + inferFrdsMap.contains(src1))
  println("inferFrdsMap.contains(src2) = " + inferFrdsMap.contains(src2))
  val prob = MCSAT(inferFrdsMap,commsMap)(src1,src2)

  println(" actual num mutual frds = " + numMutualActualFrds);
}


object DBLPTruePositive extends App{

  def findNumMutualFrds(src1: Int, src2: Int, frdsMapGlobal: scala.collection.mutable.Map[Int, ArrayBuffer[Int]]) = {
    var ret = 1;
    if(frdsMapGlobal.contains(src1) == false || frdsMapGlobal(src1) == null) ret = -1;
    if(frdsMapGlobal.contains(src2) == false || frdsMapGlobal(src2) == null) ret = -1;
    if(ret == -1) -1
    else{
      val frds1 = frdsMapGlobal(src1);
      val frds2 = frdsMapGlobal(src2);
      frds1.toSet.intersect(frds2.toSet).size
    }
  }

  val conf = ConfigFactory.load
  val datasetName = conf.getString("DataSetName");
  var (frdsMap, commsMap, backBone) = PreMain.applyDB(datasetName)

  frdsMap = Util.genFrdsMapFromDB(datasetName)

  /*
   * frdsPair consist of sample pair of two nodes, which have mutual friend number information.
   * commsPair includes the mutual community number key value.
   */
  // val (frdsPair, commsPair) = Util.sampleUniformQueryNodes(2, frdsMap, commsMap);

  // resultMap collects every source node pair, [(src1, src2), (probability, numMutualFrds)]
  val resultMap = scala.collection.mutable.HashMap[(Int, Int), (Double, Int)]();
  for(i <- 1 to conf.getInt("SampleLIMIT")){
    val frdsMapLocal = frdsMap.clone();
    var (src1, src2) = Util.genTwoKnownSrcFromDB(datasetName);
    if(src1 > src2){
      var temp = src1;
      src1 = src2
      src2 = temp;
    }
    val numMutualActualFrds = findNumMutualFrds(src1, src2, frdsMapLocal);
    if(numMutualActualFrds >= 0){
      val localGraph = RWM.apply(frdsMapLocal, backBone)(src1)
      val localGraph2 = RWM.apply(frdsMapLocal, backBone)(src2)
      val fiveSet = localGraph ++ localGraph2  ++ backBone

      /*
       * inferFrdsMap is the final fiveSet frdsMapLocal.
       */
      val inferFrdsMap = Util.prune(frdsMapLocal, fiveSet);
      println("fiveSet size = " + inferFrdsMap.size)

      val prob = MCSAT(inferFrdsMap, commsMap)(src1, src2)

      println(" actual num mutual frds = " + numMutualActualFrds);
      resultMap += (src1, src2)->(prob, numMutualActualFrds);
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  printToFile(new java.io.File("result.txt"))(p => {
    resultMap.foreach(res => p.println( " src1 and src2 = " + res._1._1 + " "+ res._1._2 + "\t" + " mutual Frd num="
    + res._2._2 +  "\t" + " prob =" +  res._2._1))
  })

  println(resultMap);
}
