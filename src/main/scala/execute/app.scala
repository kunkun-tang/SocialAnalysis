package execute

import com.typesafe.config.ConfigFactory
import preComp._
import infer._

/*
 * SocialAnalysis Main function.
 */
object socialApp extends App{

  val conf = ConfigFactory.load
  val (frdsMap, backBone) = PreMain("DBLP")

  val src1 = conf.getInt("DBLP.src1")
  val src2 = conf.getInt("DBLP.src2")

  val localGraph = PageRankWalk(frdsMap, backBone)(src1)
  val localGraph2 = PageRankWalk(frdsMap, backBone)(src2)

  val inferSet = localGraph ++ localGraph2 ++ backBone
  println("inferSet size = "+inferSet.size)
  Infer(frdsMap, inferSet)(src1, src2)
}
