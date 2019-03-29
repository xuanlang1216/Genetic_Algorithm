package genetics

import genetics.genes._
import genetics.geometry._

object GeneticAlgorithm {

  def geneticAlgorithm[T](fit:T=>Double ,gen:List[Gene]=>T,sampleGene:List[Gene]):T={
    var optimal:List[Gene] = List()
    var Geness:List[Gene]=List()
    var Tgenes:List[List[Gene]]=List()
    val NumGene:Int=sampleGene.length

    for (i <- 0 until 20){
      Geness=List()
      for(j <- 0 until NumGene){
        Geness = new Gene(math.random())::Geness
      }
      Tgenes= Geness::Tgenes
    }

    for (k <- 0 until 100000) {
      Tgenes = MergeSort.mergeSort(Tgenes, GeneComparator(fit, gen))
      optimal = Tgenes.head
      Tgenes=getNextGen(Tgenes)
    }

    gen(optimal)
  }

  def linearRegression(points:List[Point]):Line={
    val GenesToLine={input:List[Gene]=>
      val slope = math.tan((input.head.geneValue-0.5)*math.Pi)
      val yIntercept=math.tan((input.last.geneValue-0.5)*math.Pi)
      val outputLine=new Line(slope,yIntercept)
      outputLine
    }
    geneticAlgorithm(LinearFitness(points),GenesToLine,List(new Gene(0.1),new Gene(0.2)))
   }



    def LinearFitness(input:List[Point]):Line=>Double= {
      L: Line => {
        var fitness: Double = 0.0
        for (o <- input) {
          fitness += math.abs(o.y - (o.x * L.slope + L.yIntercept))
        }
        fitness
      }
    }

  // def polynomialRegression(points:List[Point],degree:Int):Polynomial={

   //}

  def getNextGen(sortedGenes:List[List[Gene]]):List[List[Gene]]={
      var nextGen:List[List[Gene]]=List()
      var Genes:List[Gene]=List()
      var NumGene:Int=0
      nextGen = sortedGenes.head::nextGen
      nextGen = Muatate(sortedGenes.head)::nextGen
      nextGen = Muatate(sortedGenes.head)::nextGen
      nextGen = Muatate(sortedGenes.apply(1))::nextGen



    for(a <- 0 to 2){
        for (b <- (a+1) to 3){
          nextGen=MakeChild(sortedGenes.apply(a),sortedGenes.apply(b))::nextGen
        }
    }

    for(k <- 0 until 10){
         Genes=List()
       for(l <- nextGen.head.indices ){
         Genes=new Gene(math.random())::Genes
       }

        nextGen=Genes::nextGen
    }
     nextGen
  }


  def Muatate(input:List[Gene]):List[Gene]={
    var mutation:List[Gene]=List()
    for (e <- input.indices){
      if((input.apply(e).geneValue*1.01)<=1.0) {
        mutation = new Gene(input.apply(e).geneValue *1.01) :: mutation
      }
      else {
        mutation = new Gene(input.apply(e).geneValue*0.99) :: mutation
      }
     }
    mutation
  }

  def MakeChild(dad:List[Gene],mom:List[Gene]):List[Gene]={
    var child:List[Gene] = List()
    for(c <- dad.indices){
      child = new Gene((dad.apply(c).geneValue+ mom.apply(c).geneValue)/2)::child
    }
    child
  }

  def GeneComparator[T](fiit:T=>Double ,geen:List[Gene]=>T):(List[Gene],List[Gene]) => Boolean ={
    (G1:List[Gene],G2:List[Gene])=>{
      fiit(geen(G1))<fiit(geen(G2))
    }
  }


  def main(args: Array[String]): Unit = {
    var xy:List[Point]=List(new geometry.Point(3,13),new geometry.Point(-1.2,-10.52),new geometry.Point(6,29.8),new geometry.Point(-5.7,-35.72))
    val myline:Line=linearRegression(xy)
    println(myline.slope+" "+myline.yIntercept)

  }


}
