package example

import doobie._
import doobie.implicits._
import Models._
import shapeless.{syntax, _}
import record._
import ops.hlist.ToList
import ops.record.{Keys, Values}

object Inserter extends App {
  val (url, xa) = PostgresStuff.go()
  val book2 = Book("authorzz", "titlezz", 1, 2.0)


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


  val create: Update0 =
    sql"""
          CREATE TABLE books (
          id SERIAL,
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
      """.query[(Int, String, String, Float)]

  val findRes = find.to[List].transact(xa).unsafeRunSync()
  println(findRes)

}
