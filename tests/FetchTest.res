open ReactTest

let placeholderText = `{"userId":1,"id":1,"title":"delectus aut autem","completed":false}`
let postPlaceholderText = `{"title":"foo","body":"bar","userId":1,"id":101}`

module Hooks = {
  let useFetch = () => {
    let (state, setState) = React.useState(() => None)

    React.useEffect0(() => {
      let promise = async () => {
        let response = await Fetch.fetch("https://jsonplaceholder.typicode.com/todos/1")
        let body = await response->Fetch.Response.json

        setState(_ => Some(body))
      }

      promise()->Promise.thenResolve(_ => ())->ignore

      None
    })

    state
  }

  let useFetchWithInit = () => {
    let (state, setState) = React.useState(() => None)

    React.useEffect0(() => {
      let promise = async () => {
        let response = await Fetch.fetchWithInit(
          "https://jsonplaceholder.typicode.com/todos/1",
          Fetch.RequestInit.make(~method_=Get, ()),
        )
        let body = await response->Fetch.Response.json

        setState(_ => Some(body))
      }

      promise()->Promise.thenResolve(_ => ())->ignore

      None
    })

    state
  }

  let usePostFetchWithInit = () => {
    let postBody = {
      "title": "foo",
      "body": "bar",
      "userId": 1,
    }
    let (state, setState) = React.useState(() => None)

    React.useEffect0(() => {
      let promise = async () => {
        let body = Fetch.BodyInit.make(JSON.stringifyAny(postBody)->Option.getOr(""))
        let headers = Fetch.HeadersInit.makeWithArray([
          ("Accept", "application/json"),
          ("Content-Type", "application/json"),
        ])
        let response = await Fetch.fetchWithInit(
          "https://jsonplaceholder.typicode.com/posts",
          Fetch.RequestInit.make(~method_=Post, ~headers, ~body, ()),
        )
        let body = await response->Fetch.Response.json

        setState(_ => Some(body))
      }

      promise()->Promise.thenResolve(_ => ())->ignore

      None
    })

    state
  }
}

module Home = {
  @react.component
  let make = (~fetchHok: unit => option<JSON.t>) => {
    let jsonPlaceholder = fetchHok()

    <div id="placeholder">
      {React.string(jsonPlaceholder->Option.mapOr("Loading...", JSON.stringify(_)))}
    </div>
  }
}

@get external textContent: Dom.element => string = "textContent"

testAsyncWithReact("fetch", (root, done) => {
  let promise = async () => {
    let _ = await act(async () => {
      root->ReactDOM.Client.Root.render(<Home fetchHok=Hooks.useFetch />)
    })

    let _ = await screen->findByText(placeholderText)
  }

  promise()
  ->Promise.thenResolve(_ => {
    Assert.elementContains(
      ~message="has result",
      ReactDOM.querySelector("#placeholder"),
      placeholderText,
    )
    done()
  })
  ->ignore
})

testAsyncWithReact("fetchWithInit", (root, done) => {
  let promise = async () => {
    let _ = await act(async () => {
      root->ReactDOM.Client.Root.render(<Home fetchHok=Hooks.useFetchWithInit />)
    })

    let _ = await screen->findByText(placeholderText)
  }

  promise()
  ->Promise.thenResolve(_ => {
    Assert.elementContains(
      ~message="has result",
      ReactDOM.querySelector("#placeholder"),
      placeholderText,
    )
    done()
  })
  ->ignore
})

testAsyncWithReact("postFetchWithInit", (root, done) => {
  let promise = async () => {
    let _ = await act(async () => {
      root->ReactDOM.Client.Root.render(<Home fetchHok=Hooks.usePostFetchWithInit />)
    })

    let _ = await screen->findByText(postPlaceholderText)
  }

  promise()
  ->Promise.thenResolve(_ => {
    Assert.elementContains(
      ~message="has result",
      ReactDOM.querySelector("#placeholder"),
      postPlaceholderText,
    )
    done()
  })
  ->ignore
})
