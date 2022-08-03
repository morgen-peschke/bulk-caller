package peschke.bulk_calls
package config

import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.apply._
import cats.syntax.traverse._
import com.typesafe.config.{Config, ConfigException, ConfigRenderOptions, ConfigValue}
import io.circe.Json
import io.circe.parser.{parse => parseJson}

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

trait ConfigReader[A] {
  def read(config: Config, path: String): Either[ConfigException, A]

  def readOpt(config: Config, path: String): Either[ConfigException, Option[A]] =
    if (config.hasPath(path)) read(config, path).map(_.some)
    else none.asRight
}
object ConfigReader {
  def apply[A](implicit CR: ConfigReader[A]): CR.type = CR

  def instance[A](r: (Config, String) => Either[ConfigException, A]): ConfigReader[A] = r(_, _)

  def instanceCatching[A](r: (Config, String) => A): ConfigReader[A] =
    instance[A] { (config, path) =>
      Either.catchOnly[ConfigException](r(config, path))
    }

  trait KeyReader[A] {
    def read(key: String): Either[ConfigException, A]
  }
  object KeyReader {
    def apply[A](implicit KR: KeyReader[A]): KR.type = KR

    def instance[A](r: String => Either[ConfigException, A]): KeyReader[A] = r(_)

    implicit val stringInstance: KeyReader[String] = _.asRight
  }

  object syntax {
    implicit final class ConfigReaderOps(private val config: Config) extends AnyVal {
      def read[A: ConfigReader](path: String): Either[ConfigException, A] = ConfigReader[A].read(config, path)

      def readOpt[A: ConfigReader](path: String): Either[ConfigException, Option[A]] =
        ConfigReader[A].readOpt(config, path)

      def at(parent: String): Either[ConfigException, ConfigReaderAtOps] = {
        config.read[Config](parent).map(_ => new ConfigReaderAtOps(config, parent))
      }
    }

    final class ConfigReaderAtOps(config: Config, parent: String) {
      def fullPath(p: String): String = s"$parent.$p"

      def read[A: ConfigReader](path: String): Either[ConfigException, A] =
        ConfigReader[A].read(config, fullPath(path))

      def readOpt[A: ConfigReader](path: String): Either[ConfigException, Option[A]] =
        ConfigReader[A].readOpt(config, fullPath(path))
    }

    implicit final class ConfigValueOps(private val configValue: ConfigValue) extends AnyVal {
      def read[A: ConfigReader]: Either[ConfigException, A] =
        configValue.atKey("dummy-key").read[A]("dummy-key")
    }
  }

  import syntax._

  implicit val stringInstance: ConfigReader[String] = ConfigReader.instanceCatching(_.getString(_))
  implicit val booleanInstance: ConfigReader[Boolean] = ConfigReader.instanceCatching(_.getBoolean(_))
  implicit val regexInstance: ConfigReader[Regex] = ConfigReader.instance { (config, path) =>
    config.read[String](path).flatMap { raw =>
      Either.catchNonFatal(raw.r).leftMap { e =>
        new ConfigException.BadValue(config.origin, path, "invalid regex", e)
      }
    }
  }
  implicit val configInstance: ConfigReader[Config] = ConfigReader.instanceCatching(_.getConfig(_))
  implicit val configValueInstance: ConfigReader[ConfigValue] = ConfigReader.instanceCatching(_.getValue(_))

  implicit def eitherInstance[A: ConfigReader, B: ConfigReader]: ConfigReader[Either[A,B]] =
    ConfigReader.instance { (config, path) =>
      config.read[A](path).map(_.asLeft).orElse(config.read[B](path).map(_.asRight))
    }

  implicit val pathReader: ConfigReader[os.Path] = ConfigReader.instance { (config, path) =>
    config.read[String](path).flatMap { raw =>
      Either.catchNonFatal(os.Path.expandUser(raw, os.pwd))
        .leftMap { e =>
          new ConfigException.BadValue(config.origin, path, "invalid path", e)
        }
    }
  }

  implicit def mapReader[K: KeyReader, V: ConfigReader]: ConfigReader[Map[K, V]] =
    ConfigReader.instance { (config, path) =>
      for {
        subConfig <- config.at(path)
        keys <- config.read[Config](path).map(_.entrySet().asScala.toList.map(_.getKey))
        entries <- keys.traverse { key =>
          (KeyReader[K].read(key), subConfig.read[V](key)).mapN(_ -> _)
        }
      } yield entries.toMap
  }

  implicit def listReader[A: ConfigReader]: ConfigReader[List[A]] = ConfigReader.instance { (config, path) =>
    config.getList(path).asScala.toList.traverse(_.read[A])
  }

  implicit val jsonReader: ConfigReader[Json] = {
    val renderAsJson =
      ConfigRenderOptions
        .defaults()
        .setComments(false)
        .setOriginComments(false)
        .setFormatted(false)
        .setJson(true)

    ConfigReader.instance { (config, path) =>
      config.read[ConfigValue](path)
        .map(_.render(renderAsJson))
        .flatMap(parseJson(_).leftMap {
          new ConfigException.BadValue(config.origin(), path, "invalid json", _)
        })
    }
  }
}
