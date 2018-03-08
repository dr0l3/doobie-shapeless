package example

import java.util.UUID

import doobie._
import doobie.implicits._
import Models._
import shapeless.{syntax, _}
import record._
import ops.hlist.ToList
import ops.record.{Keys, Values}
import shapeless.ops.product.ToTuple

object Inserter extends App {
  val (url, xa) = PostgresStuff.go()
  val book2 = Book("authorzz", "titlezz", java.util.UUID.randomUUID().toString, 2.0)


  def sqlSave[T, K <: HList, V <: HList, Repr <: HList](a: T, tableName: String)(implicit
                                                                                 labelledGeneric: LabelledGeneric.Aux[T, Repr],
                                                                                 keys: Keys.Aux[Repr, K],
                                                                                 values: Values.Aux[Repr, V],
                                                                                 ktl: ToList[K, Any],
                                                                                 vtl: ToList[V, Any]): Update0 = {

    val lgen = labelledGeneric.to(a)
    val columnNames = lgen.keys.toList.map(_.toString.substring(1))
    val valueStrings = lgen.values.toList.map(_.toString).map(v => s"'$v'")

    val f1 = fr"INSERT INTO "
    val f2 = Fragment.const(tableName)
    val f3 = Fragment.const(s"${columnNames.mkString("(", ",", ")")}")
    val f4 = Fragment.const(s"VALUES ${valueStrings.mkString("(", ",", ")")}")

    (f1 ++ f2 ++ f3 ++ f4).update
  }

  def sqlSaveRec[T, K <: HList, V <: HList, Repr <: HList](a: T, tableName: String)(implicit
                                                                                 labelledGeneric: LabelledGeneric.Aux[T, Repr],
                                                                                 keys: Keys.Aux[Repr, K],
                                                                                 values: Values.Aux[Repr, V],
                                                                                 ktl: ToList[K, Any],
                                                                                 vtl: ToList[V, Any]): Update0 = {

    val lgen = labelledGeneric.to(a)
    val columnNames = lgen.keys.toList.map(_.toString.substring(1))
    val valueStrings = lgen.values.toList.map(_.toString).map(v => s"'$v'")

    val f1 = fr"INSERT INTO "
    val f2 = Fragment.const(tableName)
    val f3 = Fragment.const(s"${columnNames.mkString("(", ",", ")")}")
    val f4 = Fragment.const(s"VALUES ${valueStrings.mkString("(", ",", ")")}")

    (f1 ++ f2 ++ f3 ++ f4).update
  }

  def findAll[T, R <: HList, K <: HList](config: Config[T])(implicit labelledGeneric: LabelledGeneric.Aux[T,R], keys: Keys.Aux[R, K], toList: ToList[K, Any], composite: Composite[T]) = {
    val keyStrings = keys().toList.map(_.toString.substring(1))
    val frag = Fragment.const(s"SELECT ${keyStrings.mkString(",")} FROM ${config.name}")
    frag.query[T]
  }
  val create: Update0 =
    sql"""
          CREATE TABLE books (
          id UUID,
          author TEXT NOT NULL,
          title TEXT,
          price FLOAT
          )
    """.update
  val createRes = create.run.transact(xa).unsafeRunSync()
  println(createRes)


  val save = sqlSave(book2, "books")
  println(save.sql)
  println(save)

  val saveRes = save.run.transact(xa).unsafeRunSync()

  println(saveRes)

  val find =
    sql"""
          select * from books
      """.query[(String, String, String, Float)]

  val findRes = find.to[List].transact(xa).unsafeRunSync()
  println(findRes)


  val books = (1 to 10).map(i => Book(s"Auhtor$i",s"title$i",java.util.UUID.randomUUID().toString,i*2.1))

  val total = books.foldLeft(sql"".update.run)((acc, book) => acc.flatMap(_ => sqlSave(book, "books").run))

  val totalRes = total.transact(xa).unsafeRunSync()
  println(totalRes)


  val findRes2 = find.to[List].transact(xa).unsafeRunSync()
  println(findRes2)

  case class Config[A](name: String)

  val conf = Config[Book]("books")

  val f = LabelledGeneric[Book]
  val t = Keys[f.Repr]
  val findRes3 = findAll(conf)

  val stuffzz = findRes3.to[List].transact(xa).unsafeRunSync()
  println(stuffzz)
}
