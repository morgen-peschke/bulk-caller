package peschke.bulk_calls
package models

import models.CallSpec._
import utils.NewType

import org.http4s.Method

/**
  * Defines a call that can be made, if the appropriate data is available
  */
sealed abstract class CallSpec(val method: Method) extends Product with Serializable {
  /**
    * A template which, when expanded, provides the URL for the call.
    */
  def url: Url

  /**
    * A list of template pairs which, when expanded, provide any query parameters for the call
    */
  def queryParams: List[(QueryParamName, QueryParamValue)]

  /**
    * A list of template pairs which, when expanded, provide any headers for the call
    */
  def extraHeaders: List[(HeaderName, HeaderValue)]

  /**
    * A specification describing the call's body.
    * @return
    */
  def body: Body
}
object CallSpec {
  object Url extends NewType[Template]
  type Url = Url.Type

  object QueryParamName extends NewType[Template]
  type QueryParamName = QueryParamName.Type

  object QueryParamValue extends NewType[Template]
  type QueryParamValue = QueryParamValue.Type

  object HeaderName extends NewType[Template]
  type HeaderName = HeaderName.Type

  object HeaderValue extends NewType[Template]
  type HeaderValue = HeaderValue.Type

  sealed abstract class Body extends Product with Serializable {
    def upcast: Body = this
  }
  object Body {
    /**
      * A template that expands into a JSON body.
      *
      * The template must produce valid JSON.
      */
    final case class JsonBody(template: Template) extends Body

    /**
      * A template that expands into a plain text body.
      */
    final case class TextBody(template: Template) extends Body

    /**
      * An explicit declaration that the call should not include a body.
      */
    final case object EmptyBody extends Body
  }

  final case class Get(url: Url,
                       queryParams: List[(QueryParamName, QueryParamValue)],
                       extraHeaders: List[(HeaderName, HeaderValue)]) extends CallSpec(Method.GET) {
    def body: Body = Body.EmptyBody
  }

  final case class Post(url: Url,
                        queryParams: List[(QueryParamName, QueryParamValue)],
                        extraHeaders: List[(HeaderName, HeaderValue)],
                        body: Body) extends CallSpec(Method.POST)
}
