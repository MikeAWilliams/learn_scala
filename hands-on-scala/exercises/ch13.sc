def markProblem(description: String) {
   println(s"\n\n------------------- $description ---------------------------------\n\n")
}

markProblem("ParallelScraping")

import $ivy.`org.jsoup:jsoup:1.13.1`, org.jsoup._
import collection.JavaConverters._

def doScrape(maxResult: Int) = {
val indexDoc = Jsoup.connect("https://developer.mozilla.org/en-US/docs/Web/API").get()
val links = indexDoc.select("h2#interfaces").nextAll.select("div.index a").asScala
val linkData = links.map(link => (link.attr("href"), link.attr("title"), link.text))

  for ((url, tooltip, name) <- linkData.take(maxResult)) yield {
     println("Scraping " + name)
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
val maxResults = 80
val (articles, duration) = time {
    doScrape(maxResults)
}
val seconds = duration.toUnit(java.util.concurrent.TimeUnit.SECONDS)
println(s"Duration $seconds seconds\n")
//assert(articles.exists(_._3 == "AnalyserNode"))
//assert(articles.exists(_._3 == "AbortController"))
//assert(articles.exists(_._3 == "AmbientLightSensor"))
println(articles.length)
assert(maxResults == articles.length)