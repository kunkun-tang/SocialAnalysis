package execute
import preComp._

/*
 * SocialAnalysis Main function.
 */
object socialApp extends App{
  val (frdsMap, backBone) = PreMain("DBLP")
  PageRankWalk(frdsMap, backBone)(36488)
  //Prob()
}
