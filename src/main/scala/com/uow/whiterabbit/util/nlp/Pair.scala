package com.uow.whiterabbit.util.nlp
import scala.collection.mutable.ListBuffer
/**Creates a Text Hypo pair with id, text string, hypo string, entailment value,
 * task, features of text and features of hypo
 * @param id of pair
 * @param text sentence string
 * @param hypo sentence string
 * @param value for decision
 * @param task for the pair (e.g. MT-eval, QA, IE)
 * @param features of the text
 * @param features of the hypo
 * */
class Pair(var id:Int, var text:String, var hypo:String, var value:String, 
      var task:String, var tasks_text:Map[String, ListBuffer[String]], 
      var tasks_hypo:Map[String, ListBuffer[String]]) extends scala.Serializable {
  def this() = this (0, "", "", "", "", Map(), Map())   
}