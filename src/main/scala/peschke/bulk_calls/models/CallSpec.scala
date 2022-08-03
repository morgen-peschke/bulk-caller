package peschke.bulk_calls
package models

import models.CallSpec._
import utils.NewType

import org.http4s.Method

sealed abstract class CallSpec(val method: Method) extends Product with Serializable {
  def url: Url
  def queryParams: List[(QueryParamName, QueryParamValue)]
  def extraHeaders: List[(HeaderName, HeaderValue)]
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
    final case class JsonBody(template: Template) extends Body
    final case class TextBody(template: Template) extends Body
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
