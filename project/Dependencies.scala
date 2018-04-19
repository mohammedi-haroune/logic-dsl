import sbt._

object Version {
  val scalaTest = "3.0.5"
}

object Library {
  val scalactic = "org.scalactic" %% "scalactic" % Version.scalaTest
  val scalatest =   "org.scalatest" %% "scalatest" % Version.scalaTest % Test
}

object Dependencies {
  import Library._

  val dsl = Seq(
    scalatest,
    scalactic
  )
}
