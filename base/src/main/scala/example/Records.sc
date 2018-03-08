import shapeless._
import shapeless.ops.record.Keys
import shapeless.record._

case class Book(author: String, title: String, id: Int, price: Double)
val book2 = Book("Benjamin Pierce","Types and Programming Languages", 262162091,44.11)
val bookHlist= Generic[Book].to(book2)

//val book =
//  ("author" ->> "Benjamin Pierce") ::
//    ("title"  ->> "Types and Programming Languages") ::
//    ("id"     ->>  262162091) ::
//    ("price"  ->>  44.11) ::
//    HNil

val lgen = LabelledGeneric[Book]

val record = lgen.to(book2)

record.keys
record.values


val something = record.keys zip record.values

val h = something.toList.head
val ttt = something.toList.tail.tail.head

h._1
ttt._1

val gen = Generic[Book]

gen.to(book2)

val s = LabelledGeneric[Book]
val kz = Keys[s.Repr]
kz()