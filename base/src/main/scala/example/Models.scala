package example

object Models {
  case class Book(author: String, title: String, id: Int, price: Double)
  case class Person(name: String, age: Int, likesIcecream: Boolean)
  case class Class(teacher: Person, pupil: Person)
  case class School(bestClass: Class, goodSchool: Boolean)
  case class City(school: School, name: String, age: Int)
}
