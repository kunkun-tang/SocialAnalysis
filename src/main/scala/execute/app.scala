package execute

import com.typesafe.config.ConfigFactory
import preComp._
import infer._
import scala.util.Random

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
  val datasetName = "LiveJournal";
  val (frdsMap_0, commsMap, backBone) = PreMain.applyDB(datasetName)

  val frdsMap = Util.genFrdsMapFromDB(datasetName)

  /*
   * frdsPair consist of sample pair of two nodes, which have mutual friend number information.
   * commsPair includes the mutual community number key value.
   */
  val (frdsPair, commsPair) = Util.sampleQueryNodes(1, frdsMap, commsMap);

  var count = 0;
  var LOCALSET_INCREMENT = 0;
  for((k,v) <- frdsPair){

	  val (src1, src2) = (v._1, v._2);

	  val localGraph = PageRankWalk.apply(frdsMap, backBone)(src1)
	  val localGraph2 = PageRankWalk.apply(frdsMap, backBone)(src2)
	  val localSet = localGraph ++ localGraph2 

	  val inferFrdsMap = Util.prune(frdsMap, localSet)
  }
 //  while(count < TEST_SOURCE_NUM){

	//   val (src1, src2) = Util.genTwoSourceNodes(frdsMap);

	//   val localGraph = PageRankWalk.apply(frdsMap, backBone)(src1)
	//   val localGraph2 = PageRankWalk.apply(frdsMap, backBone)(src2)

	//   val localSet = localGraph ++ localGraph2 
	//   LOCALSET_INCREMENT += localSet.size
	//   count += 1;
	// }

}
