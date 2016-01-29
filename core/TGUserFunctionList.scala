import scala.collection.mutable.Map
/**
  * Created by stylejy on 27/01/2016.
  */
class TGUserFunctionList {
  val keywordList = Map[String, InformationStructure]()

  def evalBody(inputName: String): Unit = {
    val eval = new TGTreeEvaluator(this, keywordList(inputName))
  }
}
