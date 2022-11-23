package entityfinder

import opennlp.tools.namefind.{NameFinderME, TokenNameFinderModel}
import opennlp.tools.postag.{POSModel, POSSample, POSTaggerME}
import opennlp.tools.tokenize.{TokenizerME, TokenizerModel}
import opennlp.tools.util.Span
import org.jsoup.Jsoup

import java.io.FileInputStream
import scala.annotation.tailrec
import scala.io.StdIn.readLine

// Named entity recognition (NER)
// Models at https://opennlp.apache.org/models.html and https://opennlp.sourceforge.net/models-1.5/
@main
def run(): Unit =
  val tokenizerModel = new TokenizerModel(
    new FileInputStream("opennlp-en-ud-ewt-tokens-1.0-1.9.3.bin")
  )
  val posModel = new POSModel(new FileInputStream("en-pos-maxent.bin"))
  val personFinderModel = new TokenNameFinderModel(new FileInputStream("en-ner-person.bin"))
  val tokenizer = new TokenizerME(tokenizerModel)
  val posFinder = new POSTaggerME(posModel)
  val personFinder = new NameFinderME(personFinderModel)
  val people = peopleInText(tokenizer, posFinder, personFinder)
  @tailrec
  def go(input: Option[String]): Unit =
    input match
      case None =>
        print("\nURL: ")
        go(Some(readLine()))
      case Some("stop") => ()
      case Some(url) =>
        val doc = Jsoup.connect(url).get
        val content = doc.selectFirst("#maincontent")
        println(content.text)
        println
        people(content.text).foreach(person => println(s"${person.name}: ${person.description}"))
        print("\nURL: ")
        go(Some(readLine()))
  go(None)

def peopleInText(tokenizer: TokenizerME, posFinder: POSTaggerME, nameFinder: NameFinderME)(
    s: String
): Seq[Person] =
  val tokens = tokenizer.tokenize(s)
  val tags = posFinder.tag(tokens)
  val names = dedup(tokens)(nameFinder.find(tokens))
  val desc = description(tokens, tags)
  names.foldLeft(Seq.empty[Person])((acc, name) =>
    acc :+ Person(content(tokens)(name).mkString(" "), desc(name))
  )

def description(tokens: Array[String], tags: Array[String])(name: Span): String =
  nounPhraseBefore(tokens, tags)(name)
    .orElse(nounPhraseAfter(tokens, tags)(name))
    .getOrElse("unknown")

val stopTokens = Seq(",", ".", "-")
val stopPos = Seq("DT", "VBD")

def nounPhraseBefore(tokens: Array[String], tags: Array[String])(tokenSpan: Span): Option[String] =
  val pos = tags.slice(0, tokenSpan.getStart).reverse.takeWhile(_ != "DT").reverse
  if pos.length < 10 then
    Some(
      tokens
        .slice(tokenSpan.getStart - pos.length - 1, tokenSpan.getStart)
        .filterNot(stopTokens.contains)
        .mkString(" ")
    )
  else None

def nounPhraseAfter(tokens: Array[String], tags: Array[String])(tokenSpan: Span): Option[String] =
  val pos =
    tags.slice(tokenSpan.getEnd + 1, tokens.length).takeWhile(token => !stopPos.contains(token))
  Some(
    tokens
      .slice(tokenSpan.getEnd, tokenSpan.getEnd + pos.length)
      .filterNot(stopTokens.contains)
      .mkString(" ")
  )

def dedup(tokens: Array[String])(tokenSpans: Array[Span]): Array[Span] =
  tokenSpans.foldLeft(Array.empty[Span])((acc, span) => {
    val contents = content(tokens)
    val accContent = acc.flatMap(contents)
    val spanContent = contents(span)
    if !(accContent.diff(spanContent) sameElements accContent) then acc
    else acc :+ span
  })

def content(tokens: Array[String])(span: Span): Array[String] =
  tokens.slice(span.getStart, span.getEnd)
