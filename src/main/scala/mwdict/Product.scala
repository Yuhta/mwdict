package mwdict

import java.net.URL
import java.nio.file.{Files, Path, Paths}

abstract class Product {
  def name: String

  def apiKey: String = sys.env("MWDICT_API_KEY_" + name.toUpperCase)
  def apiURL: URL = new URL(s"http://www.dictionaryapi.com/api/v1/references/$name/")

  def cacheHome: Path = sys.env.get("MWDICT_CACHE_HOME").fold {
    sys.env.get("XDG_CACHE_HOME").fold {
      Paths.get(sys.env("HOME")).resolve(".cache")
    } (Paths.get(_)).resolve("mwdict")
  } (Paths.get(_)).resolve(name)

  def search[A: Format](word: String)(implicit parser: Parser[this.type, A]): String = {
    val fmt = implicitly[Format[A]]
    val path = fmt.cached(this, word)
    if (Files.exists(path))
      parser.parse(fmt.content(path.toUri.toURL)).text
    else {
      val content = fmt.content(fmt.origin(this, word))
      parser.parse(content) match {
        case Parser.NoEntry(m) =>
          m
        case Parser.FoundEntry(t) =>
          // TODO Save to cache
          t
      }
    }
  }
}

object Collegiate extends Product {
  def name = "collegiate"
}
