package records.test

import org.scalatest._

import records.Rec

/*
 * Purpose of this file is to see how records behave in the IDE.
 */
object EclipseTest extends App {
  def ignoredJobs(id: String): Boolean = false
  
  val jobs = 
    List(Rec(
        "startDate" -> "2014-10-01", "jobId" -> "1234",
        "isHourlyProject" -> "Y", "hourlyRateMin" -> 10,
         "hourlyRateMax" -> 200, "hourlyRateCode" -> 100,
         "projectBudget" -> 10000, "budgetMin" -> 1000,
         "budgetMax" -> 10000, "listJob" -> "Y",
         "prevJobId" -> 100, "deleteYN" -> "Y"
    ))   
  
  case class ObjectVal(myObject: AnyRef)
  case class DBRecord(name: String, age: Int, location: String)

  // This test is used to explore the macro expansion in the IDE
  
  val x = Rec("myObject" -> "String", "foo" -> "bar")
  val x1 = Rec("myObject" -> "String", "foo" -> "bar")
  val x2 = Rec("myObject" -> "String", "foo" -> "bar")
  val x3 = Rec("myObject" -> "String", "foo" -> "bar")
  val x4 = Rec("myObject" -> "String", "foo" -> "bar")
    
  val z = x.to[ObjectVal]
  val a = if (true) Rec("a" -> 1, "b" -> 1) else Rec("a" -> 1)
  println(a.a)
  
  List(
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject,
    x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject, x.myObject
  )
  
  jobs.filter(j => !ignoredJobs(j.jobId))
   .filter(j => j.listJob == "Y" || j.jobId != "None" || j.deleteYN == "Y")
}
