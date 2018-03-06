package example

import scala.concurrent.Future

import cats._, cats.data._, cats.implicits._

object stuffzz extends App {

  def getFileNames(): Either[Throwable,List[String]] = Right(List("a", "b", "c"))
  def toUpper(str: String): Either[Throwable,String] = Right(str.toUpperCase)
  def combine(list: List[String]): String = list.mkString


  val voila = getFileNames()
    .flatMap(_.traverse(toUpper(_)))
    .map(combine)


  def lessFancy[E,T](list: List[T], func: T => Either[E, T]): Either[E, List[T]] = {
    val mapped = list.map(item => func(item))
    if(mapped.forall(_.isRight)) Right(mapped.map(_.right.get))
    else Left(mapped.find(_.isLeft).get.left.get)
  }
  val a: Either[Throwable, List[String]] = getFileNames()
  val b: Either[Throwable, List[String]] = a.flatMap(fileName => lessFancy(fileName, toUpper))
  val c: Either[Throwable, String] = b.map(fileNames => combine(fileNames))
  println(c)
}
