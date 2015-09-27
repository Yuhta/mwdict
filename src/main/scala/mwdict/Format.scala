package mwdict

import java.net.{URL, URLEncoder}
import java.nio.file.Path
import scala.xml.{Elem, XML}

object Format {
  implicit object XMLFormat extends Format[Elem] {
    def origin(product: Product, word: String) = {
      val w = URLEncoder.encode(word, "UTF-8")
      new URL(product.apiURL, s"xml/$w?key=${product.apiKey}")
    }

    def cached(product: Product, word: String) = product.cacheHome.resolve(s"$word.xml")

    def content(url: URL) = XML.load(url)
  }
}

trait Format[A] {
  def origin(product: Product, word: String): URL
  def cached(product: Product, word: String): Path
  def content(url: URL): A
}
