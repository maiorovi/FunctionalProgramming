package chapter4.either

case class Person(name:Name, age:Age)
sealed class Name(name:String)
sealed class Age(age:String)

object Person {
  def mkName(name:String):Either[String, Name] = if (name.isEmpty || name == null) Left("Name is Empty") else Right(new Name(name))

  def mkAge(age:String):Either[String, Age] = if (age.isEmpty || age == null) Left("Age is Empty") else Right(new Age(age))

  def mkPerson(name:String, age:String):Either[String,Person] = mkName(name).map2(mkAge(age))(Person(_, _))
}