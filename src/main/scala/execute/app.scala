package execute
import preComp._

/*
 * SocialAnalysis Main function.
 */
object socialApp extends App{
  val (frdsMap, backBone) = PreMain("DBLP")
  RWM(frdsMap, backBone)(100)
  Prob()
}
