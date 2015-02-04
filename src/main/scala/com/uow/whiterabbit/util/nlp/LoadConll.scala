package com.uow.whiterabbit.util.nlp
import scala.io.Source._
import scala.collection.mutable.ListBuffer
import scala.actors.remote.JavaSerializer
import java.io._
/** Load Conll style files into a hash of Pair objects
 *  @constructor Creates a new loader with the text file, hypothesis file 
 *  and definition of lines for the Conll format
 *  @param file with the features of the Text in Conll format
 *  @param file with the features of the Hypo in Conll format
 *  @param map with definitions of the columns for the Conll format (i.e. column, number)
 */
class LoadConll(var conll_file_t:String,
    var conll_file_h:String,
    var line_def:Map[String, Int] = Map("id" -> 0,
        "value" -> 1,
        "task" ->  2,
        "wordform" -> 3,
        "lemma"-> 4,
        "pos" -> 5,
        "chunk" -> 6,
        "ne" -> 7,
        "target" -> 8,
        "srl" -> 9)) {
  /**Empty constructor used when read a serial obj*/
  def this() = this("","")
  val empty = "^\\s*$".r
  var pairs:Map[Int, Pair] = Map() //map used to store pairs
  var list_text:ListBuffer[Map[Int, ListBuffer[String]]] = ListBuffer()
  var list_hypo:ListBuffer[Map[Int, ListBuffer[String]]] = ListBuffer()
  
  
  /**Process the files and store them in pairs*/
  def processFiles(): Unit = {
    readFile(this.conll_file_t, "text")
    readFile(this.conll_file_h, "hypo")
    getPairs()        
  }  
  /**Private read file 
   * @param path to file
   * @param type of file [text| hypo]
   */
  private def readFile(file:String, type_file:String): Unit = {
    var tmp:Map[Int, ListBuffer[String]] = Map()
    for(line <- fromFile(file).getLines){      
      if(empty.pattern.matcher(line).matches()){
        if(type_file.equals("text")){
          list_text += tmp
        }else if(type_file.equals("hypo")){
          list_hypo += tmp
        }
        tmp = Map()
      }else{
       val tokens = line.split("\\s+")    
       for((token, i) <- tokens.zipWithIndex){
         if(tmp.contains(i)){
           tmp(i) += token
         }else{
           tmp += (i -> ListBuffer())
           tmp(i) += token
         }
       }
      }      
    }
  }
  /**Private store lies from files into pairs with the Pair object*/
  private def getPairs(): Unit = {
    if(list_text.size == list_hypo.size){
      val size = list_text.size
      
      for(i <- 0 until size){
        val pair = new Pair()
        val tmp_t = list_text(i)
        val tmp_h = list_hypo(i)
        pair.id = tmp_t(this.line_def("id")).head.toInt 
        pair.value = tmp_t(this.line_def("value")).head
        pair.task = tmp_t(this.line_def("task")).head
        pair.text = tmp_t(this.line_def("wordform")).mkString(" ")
        pair.hypo = tmp_h(this.line_def("wordform")).mkString(" ")
        pair.tasks_text += ("wordform" -> tmp_t(this.line_def("wordform")))
        pair.tasks_hypo += ("wordform" -> tmp_h(this.line_def("wordform")))
        pair.tasks_text += ("lemma" -> tmp_t(this.line_def("lemma")))
        pair.tasks_hypo += ("lemma" -> tmp_h(this.line_def("lemma")))
        pair.tasks_text += ("pos" -> tmp_t(this.line_def("pos")))
        pair.tasks_hypo += ("pos" -> tmp_h(this.line_def("pos")))
        pair.tasks_text += ("chunk" -> tmp_t(this.line_def("chunk")))
        pair.tasks_hypo += ("chunk" -> tmp_h(this.line_def("chunk")))
        pair.tasks_text += ("ne" -> tmp_t(this.line_def("ne")))
        pair.tasks_hypo += ("ne" -> tmp_h(this.line_def("ne")))
        pair.tasks_text += ("target" -> tmp_t(this.line_def("target")).filterNot(t => 
          t == "-").map(verb => {
          val v = "srl:" + verb
          v
        }))
        pair.tasks_hypo += ("target" -> tmp_h(this.line_def("target")).filterNot(t => 
          t == "-").map(verb => {
          val v = "srl:" + verb
          v
        }))
        val start_srl = this.line_def("srl")        
        val targets_t = tmp_t(this.line_def("target")).filterNot(t => t == "-")
        val targets_h = tmp_h(this.line_def("target")).filterNot(h => h == "-")
        var n = 0
        for(j <- start_srl until tmp_t.size){
           val name = "srl:" + targets_t(n)
           pair.tasks_text += (name -> tmp_t(this.line_def("srl")))                  
           n += 1
        }
        n = 0
        for(j <- start_srl until tmp_h.size){
           val name = "srl:" + targets_h(n)           
           pair.tasks_hypo += (name -> tmp_h(this.line_def("srl")))
           n += 1
        }
        this.pairs += (pair.id -> pair)              
      }
    }
  }
  /**Write pairs to file 
   * @param file name
   * */
  def writetoFile(file:String): Unit = {
    val js = new JavaSerializer(null, null)
    val os = new DataOutputStream(new FileOutputStream(file))
    js.writeObject(os, this.pairs)
    os.close()    
  }
  /**Read pairs from file
   * @param file name
   * */
  def readfromFile(file:String): Unit = {
    val js = new JavaSerializer(null, null)
    val is = new DataInputStream(new FileInputStream(file))
    this.pairs = js.readObject(is).asInstanceOf[Map[Int, Pair]]
  }
}