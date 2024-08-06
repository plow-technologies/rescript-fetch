open Test
open Webapi.Dom

@send external remove: Dom.element => unit = "remove"

let createContainer = () => {
  let containerElement = Webapi.Dom.document->Document.createElement("div")
  let _ = containerElement->Element.setAttribute("id", "test")

  let _ =
    Webapi.Dom.document
    ->Document.asHtmlDocument
    ->Option.map(htmlDocument =>
      htmlDocument
      ->HtmlDocument.body
      ->Option.forEach(body => body->Element.appendChild(~child=containerElement))
    )

  ReactDOM.Client.createRoot(containerElement)
}

let cleanupContainer = container => {
  let containerElement = ReactDOM.querySelector("#test")
  switch containerElement {
  | Some(element) => element->remove
  | None => ()
  }
  container->ReactDOM.Client.Root.unmount()
}

let testWithReact = testWith(~setup=createContainer, ~teardown=cleanupContainer, ...)

let testAsyncWithReact = testAsyncWith(~setup=createContainer, ~teardown=cleanupContainer, ...)

// @testing-library/react
@module("@testing-library/react") external act: (unit => Promise.t<unit>) => Promise.t<unit> = "act"

@module("@@testing-library/react")
external waitFor: (unit => Promise.t<'a>) => Promise.t<'a> = "waitFor"

type screen

@module("@testing-library/react") @val
external screen: screen = "screen"

@send
external findByText: (screen, string) => Promise.t<Dom.element> = "findByText"
