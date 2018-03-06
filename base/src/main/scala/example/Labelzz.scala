package example

import cats._, cats.data._, cats.implicits._
//import cats.data._
import cats.effect._
import cats.implicits._
import doobie._
import doobie.implicits._
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist
import shapeless.ops.nat.ToInt
import shapeless.ops.{coproduct, hlist, nat}
import org.scalacheck._

object Pac {

  sealed trait SizeOf[A] {
    def value: Int
  }

  implicit def genericSizeOf[A, L <: HList, N <: Nat](
                                                       implicit generic: Generic.Aux[A, L],
                                                       size: hlist.Length.Aux[L, N],
                                                       sizeToInt: nat.ToInt[N]
                                                     ): SizeOf[A] = new SizeOf[A] {
    val value = sizeToInt.apply()
  }

  def sizeOf[A](implicit size: SizeOf[A]): Int = size.value


  trait StuffPrinter[-T] {
    def explain(t: T): String
  }

  object StuffPrinter {
    def apply[T](f: T => String) = new StuffPrinter[T]() {
      def explain(t: T) = f(t)
    }
  }

  implicit val stringExplain = StuffPrinter[String](s => s"TEXT")
  implicit val intExplain = StuffPrinter[Int](i => s"SMALLINT")
  implicit val boolExplain = StuffPrinter[Boolean](b => s"BOOL")

  implicit val hNilExplain = StuffPrinter[HNil](f => "")

  implicit def lgenExplain[T, Repr](
                                     implicit lgen: LabelledGeneric.Aux[T, Repr],
                                     reprWrites: StuffPrinter[Repr]) = StuffPrinter[T] { t =>
    reprWrites.explain(lgen.to(t))
  }

  implicit def hconExplain[Key <: Symbol, Head, Tail <: HList, L <: Nat](
                                                                          implicit key: Witness.Aux[Key],
                                                                          headStuff: Lazy[StuffPrinter[Head]],
                                                                          tailStuff: StuffPrinter[Tail],
                                                                          tLength: hlist.Length.Aux[Tail, L],
                                                                          tLengthAsInt: ToInt[L]
                                                                        )
  : StuffPrinter[FieldType[Key, Head] :: Tail] =
    StuffPrinter[FieldType[Key, Head] :: Tail] { l =>
      val headString = headStuff.value.explain(l.head)
      val tailString = tailStuff.explain(l.tail)
      s"${key.value.name} $headString${if (tLengthAsInt() > 0) "," else ""} $tailString"
      //      s"${key.value.name} -> $headString. $tailString"
    }

  implicit class StuffPrinterOps[T](t: T)(implicit stuffPrinter: StuffPrinter[T]) {
    def explain = stuffPrinter.explain(t)
  }


}

object Labelzz extends App {

  import Pac._

  case class Person(name: String, age: Int, likesIcecream: Boolean)

  case class Class(teacher: Person, pupil: Person)

  case class School(bestClass: Class, goodSchool: Boolean)

  case class City(school: School, name: String, age: Int)

  val person = Person("Rune", 31, true)

  val p1 = Person("pupil", 10, true)
  val t1 = Person("techer", 30, false)
  val c = Class(t1, p1)
  val s = School(c, true)
  val ci = City(s, "big city", 1000)
  val gen = LabelledGeneric[Person].to(person)

  val f = LabelledGeneric[Person]

  val w = Witness("helle")


  println(w)

  println(gen)


  println(1.explain)

  println(person.explain)
  println(ci.explain)

  val fff = Generic[Person]


  //for each field
  //if field is product/coproduct     -> Recursively call on said field. Save the Id.
  //else                              -> Note the type and name
  //when all fields are complete, fire the missiles.

  trait Creator[A] {
    def create(a: A, acc: Fragment): Either[Update0, Fragment]
  }

  object Creator {
    def apply[A](f: (A, Fragment) => Either[Update0, Fragment]) = new Creator[A]() {
      def create(a: A, acc: Fragment) = f(a, acc)
    }
  }

  s"""
      CREATE TABLE <A> (
        a TEXT,
        b SMALLINT,
        c BOOLEAN,
        )

   """


  implicit val stringCreator = Creator[String]((s, acc) => Right(acc ++ fr" TEXT, "))
  implicit val intCreator = Creator[Int]((i, acc) => Right(acc ++ fr" SMALLINT, "))
  implicit val boolCreator = Creator[Boolean]((b, acc) => Right(acc ++ fr" BOOLEAN, "))

  implicit val hNilCreator = Creator[HNil]((f, acc) => Left((acc ++ fr")").update))

  implicit def lgenCreator[T, Repr](
                                     implicit lgen: LabelledGeneric.Aux[T, Repr],
                                     reprWrites: Creator[Repr]) = Creator[T] { (t, acc) =>
    reprWrites.create(lgen.to(t), acc)
  }

  implicit def hconCreator[Key <: Symbol, Head, Tail <: HList, L <: Nat](
                                                                          implicit key: Witness.Aux[Key],
                                                                          headStuff: Lazy[Creator[Head]],
                                                                          tailStuff: Creator[Tail])
  : Creator[FieldType[Key, Head] :: Tail] =
    Creator[FieldType[Key, Head] :: Tail] { (l, acc) =>
      val headString = headStuff.value.create(l.head, acc)
      headString match {
        case Left(u) => Left(u)
        case Right(fr) => tailStuff.create(l.tail, fr)
      }
    }

  implicit class CreatorOps[T](t: T)(implicit creator: Creator[T]) {
    def create = creator.create(t, fr"CREATE TABLE A (")

    def lol = "heheh"
  }

  val res231 = person.create
  println(res231.left.get.sql)
}
