package example

import doobie._
import doobie.implicits._
import shapeless.{::, :+:, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.FieldType
import shapeless._

object Interfaces {
}

trait Sql

case class Value(fragment: String) extends Sql
//case class OneToOne(fieldName: String, value: String, connectionIO: ConnectionIO[Int]) extends Sql
case class Complete(list: List[(String, String)]) extends Sql

trait Saver[A] {
  def save(value: A): Sql
}

object Saver {
  def apply[A](implicit save: Saver[A]): Saver[A] = save
  def instance[A](f: A => Sql) : Saver[A] = (value: A) => f(value)

  implicit val stringSaver: Saver[String] = instance(str => Value(s"'$str'"))
  implicit val intSaver: Saver[Int] = instance(int => Value(s"${int.toString}"))
}

trait ObjectSaver[A] extends Saver[A] {
  def save(value: A): Complete
}

object ObjectSaver {
  def instance[A](f: A => Complete): ObjectSaver[A] = (value: A)=> f(value)

  implicit val hnilSaver: ObjectSaver[HNil] = instance(_ => Complete(Nil))

  implicit def hListSaver[K <: Symbol, H, T <: HList](
                                                     implicit
                                                     witness: Witness.Aux[K],
                                                     hSaver: Lazy[Saver[H]],
                                                     tSaver: ObjectSaver[T]
                                                     ): ObjectSaver[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    instance { hlist =>
      val head = hSaver.value.save(hlist.head)
      val tail = tSaver.save(hlist.tail)

      (head, tail) match {
        case (Value(f), t) => Complete((fieldName,f) :: t.list)
        case (Complete(l), t) => Complete(l ++ t.list)
        case (_,_)=> Complete(Nil)
      }
    }
  }

  implicit def genericObjectSaver[A,H](
                                      implicit
                                      labelledGeneric: LabelledGeneric.Aux[A, H],
                                      hObjectSaver: Lazy[ObjectSaver[H]]
                                      ): ObjectSaver[A] = instance { value =>
    hObjectSaver.value.save(labelledGeneric.to(value))
  }
}


object Stuff extends App {
  case class Hello(str: String, nr: Int)
//
//  val ls = the[Saver[Hello]]
//  case class World(pa: Hello)
//  val os = the[Saver[World]]
//
//  val res = os.save(World(Hello("heh", 1)))
//
//  println(res)
}