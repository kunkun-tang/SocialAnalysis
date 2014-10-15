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
  val (frdsMap, commsMap, backBone) = PreMain("DBLP")

  val TEST_SOURCE_NUM = conf.getInt("DBLP.TEST_SOURCE_NUM")

  // var src1 = conf.getInt("DBLP.src1")
  // var src2 = conf.getInt("DBLP.src2");

  var count = 0;
  var LOCALSET_INCREMENT = 0;
 //  while(count < TEST_SOURCE_NUM){

	//   val (src1, src2) = Util.genTwoSourceNodes(frdsMap);
	//   println(src1 + "  ---  "+ src2)
	//   val localGraph = PageRankWalk(frdsMap, backBone)(src1)
	//   val localGraph2 = PageRankWalk(frdsMap, backBone)(src2)

	//   val localSet = localGraph ++ localGraph2 
	//   LOCALSET_INCREMENT += localSet.size
	//   count += 1;

	//   val metrolocalGraph = RWM(frdsMap, backBone)(src1)
	//   val metrolocalGraph2 = RWM(frdsMap, backBone)(src2)

	//   val metrolocalSet = metrolocalGraph ++ metrolocalGraph2 
	//   println("metrolocalSet = " + metrolocalSet.size + " PageRank size = " + localSet.size)
	// }

	 println("localSet size = "+LOCALSET_INCREMENT/TEST_SOURCE_NUM + "  backBone set = "+ backBone.size)

}

object MetroPolispp extends App{

  val conf = ConfigFactory.load
  val (frdsMap, commsMap, backBone) = PreMain("DBLP")

  val TEST_SOURCE_NUM = conf.getInt("DBLP.TEST_SOURCE_NUM")

  // var src1 = conf.getInt("DBLP.src1")
  // var src2 = conf.getInt("DBLP.src2");

  var count = 0;
  var LOCALSET_INCREMENT = 0;
  while(count < TEST_SOURCE_NUM){

	  val (src1, src2) = Util.genTwoSourceNodes(frdsMap);

	  val localGraph = RWM(frdsMap, backBone)(src1)
	  val localGraph2 = RWM(frdsMap, backBone)(src2)

	  val localSet = localGraph ++ localGraph2 
	  LOCALSET_INCREMENT += localSet.size
	  count += 1;
	}

	 println("localSet size = "+LOCALSET_INCREMENT/TEST_SOURCE_NUM + "  backBone set = "+ backBone.size)

}


object LJPAGERANKApp extends App{

  val conf = ConfigFactory.load
  val (frdsMap, commsMap, backBone) = PreMain("LiveJournal")

  val TEST_SOURCE_NUM = conf.getInt("DBLP.TEST_SOURCE_NUM")

  // var src1 = conf.getInt("DBLP.src1")
  // var src2 = conf.getInt("DBLP.src2");

  var count = 0;
  var LOCALSET_INCREMENT = 0;
  while(count < TEST_SOURCE_NUM){

	  val (src1, src2) = Util.genTwoSourceNodes(frdsMap);
	  println(src1 + "  ---  "+ src2)
	  val localGraph = PageRankWalk(frdsMap, backBone)(src1)
	  val localGraph2 = PageRankWalk(frdsMap, backBone)(src2)

	  val localSet = localGraph ++ localGraph2 
	  LOCALSET_INCREMENT += localSet.size
	  count += 1;

	  val metrolocalGraph = RWM(frdsMap, backBone)(src1)
	  val metrolocalGraph2 = RWM(frdsMap, backBone)(src2)

	  val metrolocalSet = metrolocalGraph ++ metrolocalGraph2 
	  println("metrolocalSet = " + metrolocalSet.size + " PageRank size = " + localSet.size)
	}

	 println("localSet size = "+LOCALSET_INCREMENT/TEST_SOURCE_NUM + "  backBone set = "+ backBone.size)

}
