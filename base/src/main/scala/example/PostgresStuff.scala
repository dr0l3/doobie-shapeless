package example

import java.nio.file.Paths

import cats.effect.IO
import doobie.Transactor
import ru.yandex.qatools.embed.postgresql.EmbeddedPostgres

object PostgresStuff {
  def go(): (String, Transactor.Aux[IO, Unit]) = {
    val postgres = new EmbeddedPostgres()

    val path = Paths.get("/home/drole/.embedpostgresql")
    val url = postgres.start(EmbeddedPostgres.cachedRuntimeConfig(path))

    implicit val xa: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
      "org.postgresql.Driver", url
    )
    (url, xa)
  }
}
