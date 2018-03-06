package example

object Records extends App{
  import shapeless._
  import record._
  import ops.hlist.ToList
  import ops.record.{ Keys, Values }
  import syntax.singleton._

  def printBook[B <: HList, K <: HList, V <: HList](b : B)
                                                   (implicit
                                                    keys: Keys.Aux[B, K],
                                                    values: Values.Aux[B, V],
                                                    ktl: ToList[K, Any],
                                                    vtl: ToList[V, Any]) = {
    (b.keys.toList zip b.values.toList) foreach { case (field, value) => println(field+": "+value) }
    println
  }

  case class Book(author: String, title: String, id: Int, price: Double)
  val book2 = Book("Benjamin Pierce","Types and Programming Languages", 262162091,44.11)
  val bookHlist= Generic[Book].to(book2)
  val bookLH = LabelledGeneric[Book].to(book2)

  val book =
    ("author" ->> "Benjamin Pierce") ::
      ("title"  ->> "Types and Programming Languages") ::
      ("id"     ->>  262162091) ::
      ("price"  ->>  44.11) ::
      HNil

  printBook(book)

  println("---------------")
  println(book.keys)
  println(book.values)
  println("---------------")

  println("---------------")
  println(bookLH.keys)
  println("---------------")

//  println(bookHlist.keys)

  // Read price field
  val currentPrice = book("price")  // Static type is Double
  println("Current price is "+currentPrice)
  println

  // Update price field, relying on static type of currentPrice
  val updated = book + ("price" ->> (currentPrice+2.0))
  printBook(updated)

  // Add a new field
  val extended = updated + ("inPrint" ->> true)
  printBook(extended)

  // Remove a field
  val noId = extended - "id"
  printBook(noId)
}
