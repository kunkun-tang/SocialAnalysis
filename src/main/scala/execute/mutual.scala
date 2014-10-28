package execute

import com.typesafe.config.ConfigFactory
import preComp._
import infer._

/*
 * SocialAnalysis Main function.
 */
object mutualApp extends App{

  val conf = ConfigFactory.load
  val (frdsMap, commsMap, backBone) = PreMain.applyDB("DBLP")
  // println(frdsMap(91919))
  // val (frdsMap, backBone) = PreMain("LiveJournal")
  // CurveCommFrds(commsMap,frdsMap)
  MongoCurveMutualFrds("DBLP")
  // MongoCurveMutualFrds(frdsMap);
}
