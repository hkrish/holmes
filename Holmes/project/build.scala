import sbt._
import Keys._

object holmes extends Build {
    lazy val holmes = Project(id = "holmes",
                            base = file(".")) aggregate(crawl, search, wavelet)

    lazy val crawl = Project(id = "holmes-crawl",
                           base = file("crawl")) dependsOn(wavelet)

    lazy val search = Project(id = "holmes-search",
                           base = file("search"))

    lazy val wavelet = Project(id = "holmes-wavelet",
                           base = file("wavelet"))

}
