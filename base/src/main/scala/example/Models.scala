package example

import java.util.UUID

object Models {
  case class Book(author: String, title: String, id: String, price: Double)
  case class Person(name: String, age: Int, likesIcecream: Boolean)
  case class Class(teacher: Person, pupil: Person)
  case class School(bestClass: Class, goodSchool: Boolean)
  case class City(school: School, name: String, age: Int)
}
