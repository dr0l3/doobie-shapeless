package example

import java.nio.file.Paths

import cats._
//import cats.data._
import cats.effect._
import cats.implicits._
import doobie._
import doobie.implicits._
import ru.yandex.qatools.embed.postgresql.EmbeddedPostgres
import ru.yandex.qatools.embed.postgresql.EmbeddedPostgres._

object Go extends App {
  val postgres = new EmbeddedPostgres()

  val path = Paths.get("/home/drole/.embedpostgresql")
  val url = postgres.start(EmbeddedPostgres.cachedRuntimeConfig(path))

  println(url)

  implicit val xa: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", url
  )


  case class A(a: Int, b: String)


  //plan
  //0. making anything work
  val program1 = 42.pure[IO]
  val res = program1.unsafeRunSync()
  println(res)

  val program2: doobie.ConnectionIO[Int] = sql"select 42".query[Int].unique
  val io = program2.transact(xa).unsafeRunSync
  println(io)

  val program3: ConnectionIO[(Int, Double)] =
    for {
      a <- sql"select 42".query[Int].unique
      b <- sql"select random()".query[Double].unique
    } yield (a, b)

  val res3 = program3.transact(xa).unsafeRunSync()
  println(res3)

  //1. create table for case class

  val create: Update0 =
    sql"""
          CREATE TABLE person (
          id SERIAL,
          name TEXT NOT NULL,
          age SMALLINT
          )
    """.update
  val createRes = create.run.transact(xa).unsafeRunSync()
  println(createRes)
  //2. insert case class
  val insert =
    sql"""
          insert into person(name, age) values ('hello', 20)
      """.update
      .withUniqueGeneratedKeys("name")

  val updateRes = insert.transact(xa).unsafeRunSync()
  println(updateRes)
  //3. find case class

  val find =
    sql"""
          select * from person
      """.query[(Int,String,Int)]

  val findRes = find.to[List].transact(xa).unsafeRunSync()
  println(findRes)
  //4. update case class
  //5. delete case class

  val c1 = fr"create table "
  val c2 = fr"person "
  val c3 = fr"if not exists "
  val c4 = fr"("
  val c5 = fr"id SERIAL PRIMARY KEY,"
  val c6 = fr"name TEXT NOT NULL,"
  val c7 = fr"age SMALLINT"
  val c10 = fr")"

  val total: Fragment = c1 ++ c2 ++ c3 ++ c4 ++ c5 ++ c6 ++ c7 ++ c10

//  val create2 = total.update
//  val reszz = create2.run.transact(xa).unsafeRunSync
//  println(reszz)


  //  trait SqlStuff[A] {
//    def create: Update0
//  }
//
//  object SqlStuff {
//    def apply[A](implicit sql: SqlStuff[A]): SqlStuff[A] = sql
//  }

  //1. create table for trait
  //2. insert trait
  //3. find trait




}
