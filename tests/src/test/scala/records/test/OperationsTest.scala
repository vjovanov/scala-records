package records.test

import org.scalatest._

import records.Rec

// This is for 2.10.x compatibility!
import scala.language.reflectiveCalls

class OperationsTests extends FlatSpec with Matchers {

  "A Record" should "support the add operation" in {
    val x = Rec("a" -> 1)
    val y = x ++ (("b", 2), ("c", "s"))

    y.a should be(1)
    y.b should be(2)
    y.c should be("s")
  }
}

