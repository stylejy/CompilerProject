import instruction._
import scala.collection.mutable.Map
/**
  * Created by stylejy on 25/01/2016.
  */
class TGKeywordList {
  val keyworkdList = Map[String, Object]()

  keyworkdList += ("defn" -> TGdefn)
  keyworkdList += ("if" -> TGif)
}
