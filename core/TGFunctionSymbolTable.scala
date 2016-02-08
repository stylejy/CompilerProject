import scala.collection.mutable.Map

/**
  * Created by stylejy on 27/01/2016.
  */
class TGFunctionSymbolTable {
  //First tuple is for args list and second one is for the body
  val list = Map[String, (Expr, Expr)]()
  /*var oldSymbolTable = this

  def evalBody(inputName: String): Unit = {
    //new TGTreeEvaluator(this, keywordList(inputName)).run
  }

  def setOldSymbolTable(inputOldSymbolTable: TGSymbolTable): Unit = {
    oldSymbolTable = inputOldSymbolTable
  }

  def enterNewScope: TGSymbolTable = {
    val newScope = new TGSymbolTable()
    newScope.setOldSymbolTable(this)
    newScope
  }

  def exitCurrentScope: TGSymbolTable ={
    oldSymbolTable
  }*/
}
