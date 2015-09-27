package mwdict

object Parser {
  sealed trait Result {
    def text: String
  }

  case class NoEntry(text: String) extends Result
  case class FoundEntry(text: String) extends Result
}

trait Parser[A, B] {
  def parse(source: B): Parser.Result
}
