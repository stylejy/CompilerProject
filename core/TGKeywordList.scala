import scala.collection.mutable.Map
/**
  * Created by stylejy on 25/01/2016.
  */
class TGKeywordList {
  val keywordList = Map[String, Boolean]()

  keywordList += ("defn" -> true)
  keywordList += ("if" -> true)
  keywordList += ("or" -> true)
  keywordList += ("=" -> true)
  keywordList += ("+" -> true)
  keywordList += ("-" -> true)
}
