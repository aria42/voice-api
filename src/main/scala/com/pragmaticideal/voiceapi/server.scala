package com.pragmaticideal.voiceapi

import org.scalatra._
import org.scalatra.json._
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.scalate.ScalateSupport

import scala.util.{Try, Success}

// Everything in this servlet is a json API
class JsonAPIServlet extends ScalatraServlet with JacksonJsonSupport  {

  // Sets up automatic case class to JSON output serialization
  protected implicit val jsonFormats: Formats = DefaultFormats

  // Assume all content types, unless override, are JSON
  before() {
    contentType = formats("json")
  }

  case class SomeData(val name: String, val count: Long)

  val dataElems = Seq(
    SomeData("d1", 1),
    SomeData("d2", 2),
    SomeData("d3", 3)
  )

  get("/hello/:id") {
    // Try is like an option for throwable expressions
    // only return result if parse as int and in bounds
    Try(Integer.parseInt(params("id"))) match {
      // Results automatically converted to JSON
      case Success(id : Int) if id >= 0 &&  id < dataElems.length
          => dataElems(id)
      case _ => NotFound(s"Don't have ${params("id")}")
    }
  }
}

// Sample for a webapp
class WebappServlet extends ScalatraServlet with ScalateSupport {

  before() {
    contentType = "text/html"
  }

  get("/") {
    // This will look up index.ssp inside of "/webapp/WEB-INF/templates/index.ssp"
    ssp("/index", "title" -> "Some page title", "body" -> "Mmm, my body")
  }
}

