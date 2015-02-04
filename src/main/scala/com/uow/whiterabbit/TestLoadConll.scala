package main.scala.com.uow.whiterabbit
import com.uow.whiterabbit.util.nlp._
object TestLoadConll {
/**Test for LoadConll and Pair
   * 1 process files
   * 2 store pairs to file or work with them
   * 3 read pairs from file
   * */
  def main(args:Array[String]): Unit ={
    val file_h = "/media/raid-vapnik/mrios/workspace/factorie/workspace/whiterabbit/src/main/scala/com/uow/whiterabbit/datasets/rte1_dev.hypo"
    val file_t = "/media/raid-vapnik/mrios/workspace/factorie/workspace/whiterabbit/src/main/scala/com/uow/whiterabbit/datasets/rte1_dev.text"
    val load = new LoadConll(file_t, file_h) //create loader
    load.processFiles() //process Conll files
    println(load.pairs.size)
    //load.pairs.values.foreach(pair => println(pair.id + "\t" + pair.features_hypo("target") +
    //    "\n\t" + pair.text + "\n\t" + pair.hypo))
    load.writetoFile("/media/raid-vapnik/mrios/workspace/factorie/workspace/whiterabbit/src/main/scala/com/uow/whiterabbit/datasets/rte1_dev.ser") //write to file
    val load_ser = new LoadConll() //empty constructor we don't need to process files again
    load_ser.readfromFile("/media/raid-vapnik/mrios/workspace/factorie/workspace/whiterabbit/src/main/scala/com/uow/whiterabbit/datasets/rte1_dev.ser") //read from file
    load_ser.pairs.values.foreach(pair => {
      println(pair.id + "\t" + pair.tasks_text("wordform")+
        "\t" + pair.tasks_hypo("wordform")  +
        "\n\t" + pair.text + "\n\t" + pair.hypo)
      //val pt = new PairTool(pair)
      //val w_frame = pt.tagToToken("lemma", "srl", "hypo")
      //println("WtoFRAME:" + w_frame)
      
      
      if(pair.tasks_hypo("target").length != 0){
        val verb = pair.tasks_hypo("target").head
        val pt = new PairTool(pair)
        val w_frame = pt.tagToToken("lemma", verb, "hypo")
        
        //println(pair.features_hypo.keys.toList.toString)
        println("VERB:"+ verb + "\t" + pair.tasks_hypo(verb))
        println("WtoFRAME:" + w_frame)
        //val tool = new PairTool(pair)
        //val srl = tool.tagToToken("lemma",verb,"hypo")
        //println("**********")
        //println(tool.tag_token.keys.toList.toString + "\n" 
        //    + tool.tag_token.values.toList.toString)
      }      
    })
        
  }
}