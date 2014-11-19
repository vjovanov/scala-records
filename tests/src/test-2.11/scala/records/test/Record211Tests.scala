package records.test

import org.scalatest._

import records.Rec

import scala.collection.mutable

class Record211Tests extends FlatSpec with Matchers {

  case class DBRecord(name: String, age: Int, location: String)

  "A Record" should "not depend on declared field order" in {

    val people = List(
      Rec("age" -> 1, "name" -> "Michael"),
      Rec("name" -> "Ahir", "age" -> 23))

    people.head.name should be ("Michael")
    people.last.name should be ("Ahir")

  }

  it should "be able to convert to complex case classes" in {

    val data = List(
      Rec("name" -> "Hans",  "age" -> 256, "location" -> "home"),
      Rec("name" -> "Peter", "age" ->   1, "location" -> "bed"),
      Rec("name" -> "Chuck", "age" ->   2, "location" -> "bar"))

    val recs = data.map(_.to[DBRecord])

    recs should be (List(
      DBRecord("Hans",  256, "home"),
      DBRecord("Peter",   1,  "bed"),
      DBRecord("Chuck",   2,  "bar")))
  }

  it should "lub for records of different shapes" in {

    val data = List(
      Rec("name" -> "Hans"),
      Rec("name" -> "Peter", "age" ->   1),
      Rec("name" -> "Chuck", "age" ->   2, "location" -> "bar"))

    val recs = data.map(_.name)

    recs should be (List("Hans", "Peter", "Chuck"))
  }

  it should "lub different records in different contexts" in {
    val x = List(Rec("age" -> 2, "name" -> "Heather"), Rec("age" -> 3, "name" -> "Tobias"))

    x.head.age should be (2)
    x.last.age should be (3)

    val r = if (true) Rec("age" -> 2, "name" -> "Tobias") else Rec("age" -> 1, "name" -> "Heather")
    r.age should be (2)

    val r1 = true match {
      case true => Rec("age" -> 3, "name" -> "Hubert")
      case false => Rec("age" -> 3, "name" -> "Hubert")
    }
    r1.age should be (3)
  }

  it should "work in implicitly typed sets" in {

    val set = mutable.Set(Rec("a" -> 1, "b" -> 2))

    set += Rec("a" -> 2, "b" -> 3)
    set += Rec("a" -> 5, "b" -> 3)

    assert(set.contains(Rec("a" -> 1, "b" -> 2)))
    assert(set.contains(Rec("a" -> 2, "b" -> 3)))
    assert(set.contains(Rec("a" -> 5, "b" -> 3)))

    assert(!set.contains(Rec("a" -> 7, "b" -> 9)))
    assert(!set.contains(Rec("a" -> 7, "b" -> 10)))
  }

  import org.scala_lang.virtualized._
  trait Struct

  case class Const[T](x: T) extends Rep[T] {
    val fields = Nil
  }
  case class IntConst(x: Int) extends Rep[Int] {
   val fields = Nil
  }
  implicit def unit[T](x: T): Rep[T] = Const(x)
  def newStruct[T](v: (String, Rep[Any])*): Rep[T] = {
    new Rep[T]{
      val fields: List[(String, Rep[Any])] = v.toList
      override def equals(x: Any): Boolean = {
        x.isInstanceOf[Rep[T]] && x.asInstanceOf[Rep[T]].fields == fields
      }
    }
  }

  it should "work with basic rep and non-rep types" in {
    val res: Rep[Struct{ val name: String; val age: Int }] =
      Record(name = "Hannah", age = Const(30))
    res.fields should be(List(("name", Const("Hannah")), ("age", Const(30))))
    // res.name should be ("Hannah")
  }

  it should "support nested records" in {
    val res: Rep[Struct{ val name: String; val age: Int; val country: Struct {val name: String} }] =
      Record(name = Const("Hannah"), age = 30, country = Record(name = "US"))
    res.fields should be(List(("name", Const("Hannah")), ("age", Const(30)), ("country", Record(name = "US"))))
  }

}
