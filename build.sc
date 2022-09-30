import mill._
import scalalib._
import scalafmt._
// support BSP
// input build.sc from each repositories.
import $file.`fudian`.build

object ivys {
  val sv = "2.12.15"
  val upickle = ivy"com.lihaoyi::upickle:1.3.15"
  val oslib = ivy"com.lihaoyi::os-lib:0.7.8"
  val pprint = ivy"com.lihaoyi::pprint:0.6.6"
  val utest = ivy"com.lihaoyi::utest:0.7.10"
  val macroParadise = ivy"org.scalamacros:::paradise:2.1.1"
  val jline = ivy"org.scala-lang.modules:scala-jline:2.12.1"
  val scalatest = ivy"org.scalatest::scalatest:3.2.2"
  val scalatestplus = ivy"org.scalatestplus::scalacheck-1-14:3.1.1.1"
  val scalacheck = ivy"org.scalacheck::scalacheck:1.14.3"
  val scopt = ivy"com.github.scopt::scopt:3.7.1"
  val playjson =ivy"com.typesafe.play::play-json:2.6.10"
  val spire = ivy"org.typelevel::spire:0.16.2"
  val breeze = ivy"org.scalanlp::breeze:1.1"

  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.5.0"
  val chisel3Plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.5.0"
  val chiseltest = ivy"edu.berkeley.cs::chiseltest:0.5.0"
  val chiselCirct = ivy"com.sifive::chisel-circt:0.4.0"
}

object fpuv2 extends SbtModule with ScalaModule with ScalafmtModule {
  override def millSourcePath = millOuterCtx.millSourcePath
  override def scalaVersion = ivys.sv

  override def ivyDeps = Agg(
    ivys.chisel3,
    ivys.chiseltest,
    ivys.chiselCirct
  )

  override def compileIvyDeps = Agg(ivys.macroParadise)

  override def scalacPluginIvyDeps = Agg(
    ivys.macroParadise,
    ivys.chisel3Plugin
  )

  override def scalacOptions = Seq("-Xsource:2.11")

  override def moduleDeps = Seq(`fudian`.build.fudian)

  object test extends Tests with TestModule.ScalaTest {

    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivys.chiseltest,
      ivys.scalatest
    )
  }
}