package main.scala.com.uow.whiterabbit
import com.uow.whiterabbit.util.nlp._
import cc.factorie._
import cc.factorie.la._         // Linear algebra
import cc.factorie.optimize._
import scala.collection.mutable.ListBuffer
object loglinearRTE {
  class TokPair(tstr:String, hstr:String) // TODO text string and hypo string
  object PairDomain extends CategoricalDomain[String] //Domain of objects
  class Pair(str:String) extends CategoricalVariable(str) { def domain = PairDomain }
  object LabelDomain extends CategoricalDomain(List(new BooleanVariable(true), new BooleanVariable(false)))
  class PairSeq extends scala.collection.mutable.ArrayBuffer[Pair]
  class Label(dec:BooleanVariable, val p:PairSeq) extends LabeledCategoricalVariable(dec) { def domain = LabelDomain }
  class LabelSeq extends scala.collection.mutable.ArrayBuffer[Label]
   // TODO several labels for each example (labelSeq) or one label????     
   
  def getSeq(text:ListBuffer[String] ,hypo:ListBuffer[String], v:BooleanVariable) = {
    val pair_seq = new PairSeq()
    for(t <- text){
      for(h <- hypo){
       val combo = t + "|||" + h
       pair_seq.append(new Pair(combo))
      }
    }
    pair_seq
    //val label_seq = new LabelSeq()
    //label_seq.append(new Label(v, pair_seq))
    //label_seq.foreach(f => println(f.p))
    //List(label_seq) 
  }   
   
  /*def printLabels(matrix:LabelSeq): Unit ={
	  for(row <- matrix){
        for(col <- row){
          println(col.p + "\t" + col.value)
      }
        println()
      }
  }*/
   
   val model = new Model with Parameters {
     //TODO use another structure to override the score
	 //possible structure: TupleFactorWithStatistics2
     //TODO bias for class label
	 val observ = new DotFamilyWithStatistics2[Label,Pair] { 
	   val weights = Weights(new la.DenseTensor2(LabelDomain.size, PairDomain.size))
		   /*override def score(l:Label#Value, pair:Pair#Value) = {
		     println(l + " " + pair)
		     val words = pair.toString.split("|||")
		     println("W" + words)
		     if(words.head == words.tail) 1.0 else 0.0
		  }*/
	 } 
	 override def factors(labels:Iterable[Var]) = labels match {
	   //TODO list of factors for each pair!!! not just the HEAD
	   //for(pair <- label.p) new observ.Factor(label, p)
	       case labels:LabelSeq => labels.flatten(label =>  for(pair <- label.p) yield new observ.Factor(label, pair)) 
	 }
   }
   
  /* val model_template = new TemplateModel with Parameters {
    addTemplates(
      
      // Factor between label and observed token
      new DotTemplateWithStatistics2[BioConllNerLabel,TokenFeatures] {
        //def statisticsDomains = ((Conll2003NerDomain, TokenFeaturesDomain))
        val weights = Weights(new la.DenseTensor2(BioConllNerDomain.size, TokenFeaturesDomain.dimensionSize))
        def unroll1(label: BioConllNerLabel) = Factor(label, label.token.attr[TokenFeatures])
        def unroll2(tf: TokenFeatures) = Factor(tf.token.attr[BioConllNerLabel], tf)
      }
    )
   }*/
  
   def main(args:Array[String]): Unit = {
     val load_ser_train = new LoadConll() //empty constructor we don't need to process files again
     load_ser_train.readfromFile(args(0)) //read from file
     val load_ser_test = new LoadConll() //empty constructor we don't need to process files again
     load_ser_test.readfromFile(args(1)) //read from file
     //val v = new BooleanVariable(true)
     //val label_seq_train = List()
     //val label_seq_test = List()
     var max = args(2).toInt
     if(max > load_ser_train.pairs.values.size){
       max = load_ser_train.pairs.values.size
     }
     val label_seq_train = load_ser_train.pairs.values.slice(0,max).map(pair => {
       val text = pair.tasks_text("wordform")
       val hypo = pair.tasks_hypo("wordform")
       println(text)
       println(hypo)
       val value = pair.value
       val v = new BooleanVariable()       
       if(value == "1"){
         v := true
       }else{
         v := false
       }
       val seq = getSeq(text, hypo, v)
       val label_seq = new LabelSeq()       
       label_seq.append(new Label(v, seq))
       label_seq
     })
     
     val label_seq_test = load_ser_test.pairs.values.slice(0,max).map(pair => {
       val text = pair.tasks_text("wordform")
       val hypo = pair.tasks_hypo("wordform")
       val value = pair.value
       val v = new BooleanVariable()       
       if(value == "1"){
         v := true
       }else{
         v := false
       }
       val seq = getSeq(text, hypo, v)
       //println(v,seq)
       val label_seq = new LabelSeq()       
       label_seq.append(new Label(v, seq))
       label_seq
     })
     //println(label_seq_test.slice(0,1))
     val trainer = new OnlineTrainer(model.parameters, new optimize.AdaGrad())
     //val trainer1 = new BatchTrainer(model.parameters, new optimize.AdaGrad())
     //val list_train = List(label_seq_train.slice(0,10))
     //val list_test = List(label_seq_test.slice(0,10))
     //println(list_train)
     val t0 = System.currentTimeMillis()
     //InferBy is marginalization
     //MaximizeBy are MAP inference algorithms
     trainer.trainFromExamples(label_seq_train.map(f => new LikelihoodExample(f, model,InferByBPChainSum))) 
     //trainer1.trainFromExamples(label_seq_train.map(f => new LikelihoodExample(f, model,InferByBPChainSum)))
     val t1 = System.currentTimeMillis()
     println("Elapsed time training: " + (t1 - t0) + "ms")
     label_seq_train.foreach(f => BP.inferChainMax(f, model))
     label_seq_test.foreach(f => BP.inferChainMax(f, model))
     println(label_seq_train)
     println(label_seq_test)
     //printLabels(label_seq_train)
     //printLabels(label_seq_test)  
     // TODO param label of likelihood is wrong 
     //val example = new optimize.LikelihoodExample(label_seq_train.toSeq.map(struct => struct.head), model, InferByBPChainSum)
     //val optimizer0 = new optimize.AdaGrad()
     //Trainer.onlineTrain(model.parameters, Seq(example), optimizer=optimizer0)(random: scala.util.Random)
     //label_seq_test.foreach(f => BP.inferChainMax(f, model))
     //val trainer = new OnlineTrainer(model.parameters, optimizer0)     
     //trainer.trainFromExamples(Seq(example))
     //while (!trainer.isConverged){
      //trainer.processExamples(Seq(example))
     //}
   }
}