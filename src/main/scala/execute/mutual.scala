package execute

import com.typesafe.config.ConfigFactory
import preComp._
import infer._

/*
 * SocialAnalysis Main function.
 */
object mutualApp extends App{

  val conf = ConfigFactory.load
  val (frdsMap, commsMap, backBone) = PreMain("DBLP")
  // val (frdsMap, backBone) = PreMain("LiveJournal")
  // CurveCommFrds(commsMap,frdsMap)
  CurveMutualFrds(frdsMap)
}
