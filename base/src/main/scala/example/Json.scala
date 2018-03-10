package example


sealed trait JsonValue

case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue

case class JsonArray(items: List[JsonValue]) extends JsonValue

case class JsonString(value: String) extends JsonValue

case class JsonBoolean(value: Boolean) extends JsonValue

case object JsonNull extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

/**
  * Encoder for Scala types. These are elements within the generic HList represents.
  */
object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

  def createEncoder[A](f: A => JsonValue): JsonEncoder[A] =
    new JsonEncoder[A] {
      def encode(value: A): JsonValue = f(value)
    }

  /* Instance encoders. */
  implicit val stringEncoder: JsonEncoder[String] =
    createEncoder(str => JsonString(str))
//  implicit val doubleEncoder: JsonEncoder[Double] =
//    createEncoder(num => JsonNumber(num))
//  implicit val IntEncoder: JsonEncoder[Int] =
//    createEncoder(num => JsonNumber(num))
  implicit val boolEncoder: JsonEncoder[Boolean] =
    createEncoder(bool => JsonBoolean(bool))

  /* Instance combinators. */
  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(xs => JsonArray(xs.map(enc.encode)))

  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))
}

import shapeless._
import shapeless.labelled.FieldType

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}

/**
  * Encoder for (labelled) generic `HList`s.
  */
object JsonObjectEncoder {
  def createObjectEncoder[A](f: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      def encode(value: A): JsonObject = f(value)
    }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    createObjectEncoder(_ => JsonObject(Nil))

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
                                                         implicit
                                                         witness: Witness.Aux[K],
                                                         hEncoder: Lazy[JsonEncoder[H]], // `Lazy` important because JsonObject is recursive, prevent implicit divergence
                                                         tEncoder: JsonObjectEncoder[T]
                                                       ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    createObjectEncoder { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      JsonObject(fieldName -> head :: tail.fields)
    }
  }

  implicit val cnilEncoder: JsonObjectEncoder[CNil] =
    createObjectEncoder(_ => throw new Exception("Impossible!"))

  implicit def coproductEncoder[K <: Symbol, H, T <: Coproduct](
                                                                 implicit
                                                                 witness: Witness.Aux[K],
                                                                 hEncoder: Lazy[JsonEncoder[H]], // `Lazy` important because JsonObject is recursive, prevent implicit divergence
                                                                 tEncoder: JsonObjectEncoder[T]
                                                               ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
    val fieldName = witness.value.name
    createObjectEncoder {
      case Inl(h) =>
        JsonObject(List(fieldName -> hEncoder.value.encode(h)))
      case Inr(t) =>
        tEncoder.encode(t)
    }
  }

  implicit def genericObjectEncoder[A, H](
                                           implicit
                                           generic: LabelledGeneric.Aux[A, H],
                                           hEncoder: Lazy[JsonObjectEncoder[H]]
                                         ): JsonEncoder[A] =
    createObjectEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }
}

object Do extends App{
  case class Hello(str: String, nr: Boolean)
  val f = implicitly[JsonEncoder[Hello]]
  case class World(h: Hello)

  val t = implicitly[JsonEncoder[World]]

  val res = t.encode(World(Hello("hehe",true)))

  println(res)
}
