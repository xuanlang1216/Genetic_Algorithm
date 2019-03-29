package tests

import org.scalatest._
import genetics._

class TestLinearRegression extends FunSuite {

  test("linear1 "){
    var points:List[geometry.Point]=List(new geometry.Point(1,1),new geometry.Point(2,2),new geometry.Point(3,3),new geometry.Point(4,4))
    var OutputLine:geometry.Line=GeneticAlgorithm.linearRegression(points)
    assert(math.abs(OutputLine.slope-1.0)<0.05,"slope")
    assert(math.abs(OutputLine.yIntercept-0)<0.05,"yIntercept")
  }

  test("linear2 "){
    var points2:List[geometry.Point]=List(new geometry.Point(3,13),new geometry.Point(-1.2,-10.52),new geometry.Point(6,29.8),new geometry.Point(-5.7,-35.72))
    var OutputLine2:geometry.Line=GeneticAlgorithm.linearRegression(points2)
    assert(math.abs(OutputLine2.slope-5.6)<0.05,"slope")
    assert(math.abs(OutputLine2.yIntercept+3.8)<0.5,"yIntercept")
  }

}
