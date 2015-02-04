package com.uow.whiterabbit.util.nlp
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap
/**Creates an object that merges words with SRL tags
 * @constructor Creates a tool that support operations over the features  
 * @param Pair p object
 * */
class PairTool(p:Pair) {
 
  private var tag_token:LinkedHashMap[String,ListBuffer[String]] = LinkedHashMap() //insertion order hash
  private var x:ListBuffer[String] = ListBuffer()
  private var tags:ListBuffer[String] = ListBuffer()
  
  /**Merges the two lists x and tags
   * @param x_name string with the column to extract tokens to merge
   * @param tags_name string with the column with the SRL tags
   * @param type_name string with the part name from where to extract columns 
   * 		(i.e. text or hypo) default [text]
   * @return map with pairs of the type tag-chunk
   * */
  def tagToToken(x_name:String, tags_name:String,
      type_name:String = "text"):LinkedHashMap[String,ListBuffer[String]] = {
    var tmp:ListBuffer[String] = ListBuffer()
    extractCols(x_name, tags_name, type_name)
    for((tag, i) <- tags.zipWithIndex){
      //println("tag: " + tag)
      if(!tag.startsWith("O")){        
        val bio = tag.split("-")           
        if(bio.head.startsWith("B") || bio.head.startsWith("I")){           
            tmp += x(i)
          }else if(bio.head.startsWith("E")){            
            tmp += x(i)
            //println(tmp.toList.toString)
            this.tag_token += (bio.tail.mkString("-") -> tmp)            
            tmp = ListBuffer()
          }else if(bio.head.startsWith("S")){            
            tmp += x(i)
            this.tag_token += (bio.tail.mkString("-") -> tmp)           
            tmp = ListBuffer()
        }        
      }
    }
    this.tag_token
  }
  
  private def extractCols(x:String, y:String, name:String) = {
    if(name.equals("text")){
      this.x = p.tasks_text(x)
      this.tags = p.tasks_text(y)
    }else{
      this.x = p.tasks_hypo(x)
      this.tags = p.tasks_hypo(y)
    }
  }
}