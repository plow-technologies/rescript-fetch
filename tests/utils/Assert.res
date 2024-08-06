open Test

@get external textContent: Dom.element => string = "textContent"

let isSome = (~message: string, value: option<'a>) =>
  switch value {
  | Some(_) => pass(~message, ())
  | None => fail(~message="Expected Some, got None", ())
  }

let elementContains = (~message=?, element: option<Dom.element>, substring: string) =>
  assertion(
    ~message?,
    ~operator="elementContains",
    (textContent, substring) => {
      textContent->String.includes(substring)
    },
    element->Option.mapOr("Not Found", element => element->textContent),
    substring,
  )
