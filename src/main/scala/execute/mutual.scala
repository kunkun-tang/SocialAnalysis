package execute

import com.typesafe.config.ConfigFactory
import preComp._
import infer._

/*
 * SocialAnalysis Main function.
 */
object mutualApp extends App{

  val conf = ConfigFactory.load

  val datasetName = "DBLP";
  val (frdsMap, commsMap, backBone) = PreMain.applyMemory(datasetName)
  // println(frdsMap(91919))
  // val (frdsMap, backBone) = PreMain("LiveJournal")
  CurveMutualFrds(frdsMap)
  // MongoCurveMutualFrds(datasetName)
  // MongoCurveCommFrds(datasetName, commsMap)
  // MongoCurveMutualFrds(frdsMap);
}
