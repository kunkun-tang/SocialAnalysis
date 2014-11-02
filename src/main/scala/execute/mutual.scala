package execute

import com.typesafe.config.ConfigFactory
import preComp._
import infer._

/*
 * SocialAnalysis Main function.
 */
object mutualApp extends App{

  val conf = ConfigFactory.load

  val datasetName = "LiveJournal";
  val (frdsMap, commsMap, backBone) = PreMain.applyDB(datasetName)
  // println(frdsMap(91919))
  // val (frdsMap, backBone) = PreMain("LiveJournal")
  // CurveCommFrds(commsMap,frdsMap)
  // MongoCurveMutualFrds(datasetName)
  MongoCurveCommFrds(datasetName, commsMap)
  // MongoCurveMutualFrds(frdsMap);
}
