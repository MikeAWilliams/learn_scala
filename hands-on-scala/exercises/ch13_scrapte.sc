def markProblem(description: String) {
   println(s"\n\n------------------- $description ---------------------------------\n\n")
}

markProblem("ParallelScraping")

import scala.concurrent._, java.util.concurrent.Executors
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import $ivy.`org.jsoup:jsoup:1.13.1`, org.jsoup._
import collection.JavaConverters._

implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

def doScrape(maxResult: Int) = {
  val indexDoc = Jsoup.connect("https://developer.mozilla.org/en-US/docs/Web/API").get()
  val links = indexDoc.select("h2#interfaces").nextAll.select("div.index a").asScala
  val linkData = links.map(link => (link.attr("href"), link.attr("title"), link.text))

  for ((url, tooltip, name) <- linkData.take(maxResult)) yield Future{
    //println("     Scraping " + name)
    val doc = Jsoup.connect("https://developer.mozilla.org" + url).get()
    val summary = doc.select("article#wikiArticle > p").asScala.headOption match {
        case Some(n) => n.text; case None => ""
    }
    val methodsAndProperties = doc
        .select("article#wikiArticle dl dt")
        .asScala
        .map(el => (el.text, el.nextElementSibling match {case null => ""; case x => x.text}))
    (url, tooltip, name, summary, methodsAndProperties)
  }
}
val maxResults = 400
val (articleFutures, duration) = time {
    doScrape(maxResults)
}
val seconds = duration.toUnit(TimeUnit.SECONDS)
println(s"time to get futures $seconds seconds")
println(s"Lenght of futures ${articleFutures.length}")
assert(maxResults == articleFutures.length)

val (articles, awaitDuration) = time {
    articleFutures.map(Await.result(_, Duration.Inf))
}
val secondsAwait = awaitDuration.toUnit(TimeUnit.SECONDS)
println(s"time to get restult $secondsAwait seconds")