type body
type bodyInit
type headers
type headersInit
type response
type request
type requestInit

// external
type arrayBuffer /* TypedArray */
type blob /* FileAPI */
type bufferSource /* Web IDL, either an arrayBuffer or arrayBufferView */
type formData /* XMLHttpRequest */
type readableStream /* Streams */
type urlSearchParams /* URL */
type abortController
type signal

type requestMethod =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch
  | Other(string)

/* internal */
let encodeRequestMethod = (requestMethod: requestMethod): string =>
  switch requestMethod {
  | Get => "GET"
  | Head => "HEAD"
  | Post => "POST"
  | Put => "PUT"
  | Delete => "DELETE"
  | Connect => "CONNECT"
  | Options => "OPTIONS"
  | Trace => "TRACE"
  | Patch => "PATCH"
  | Other(method_) => method_
  }

/* internal */
let decodeRequestMethod = (requestMethod: string): requestMethod =>
  switch requestMethod {
  | "GET" => Get
  | "HEAD" => Head
  | "POST" => Post
  | "PUT" => Put
  | "DELETE" => Delete
  | "CONNECT" => Connect
  | "OPTIONS" => Options
  | "TRACE" => Trace
  | "PATCH" => Patch
  | method_ => Other(method_)
  }

module AbortController = {
  /* Experimental API */
  type t = abortController

  /* Experimental API */
  @get
  external signal: t => signal = "signal"

  /* Experimental API */
  @send
  external abort: t => unit = "abort"

  /* Experimental API */
  @new
  external make: unit => t = "AbortController"
}

type referrerPolicy =
  | None
  | NoReferrer
  | NoReferrerWhenDowngrade
  | SameOrigin
  | Origin
  | StrictOrigin
  | OriginWhenCrossOrigin
  | StrictOriginWhenCrossOrigin
  | UnsafeUrl

/* internal */
let encodeReferrerPolicy = (referrerPolicy: referrerPolicy) =>
  switch referrerPolicy {
  | None => ""
  | NoReferrer => "no-referrer"
  | NoReferrerWhenDowngrade => "no-referrer-when-downgrade"
  | SameOrigin => "same-origin"
  | Origin => "origin"
  | StrictOrigin => "strict-origin"
  | OriginWhenCrossOrigin => "origin-when-cross-origin"
  | StrictOriginWhenCrossOrigin => "strict-origin-when-cross-origin"
  | UnsafeUrl => "unsafe-url"
  }

exception UnknownReferrerPolicy(string)

/* internal */
let decodeReferrerPolicy = (referrerPolicy: string) =>
  switch referrerPolicy {
  | "" => None
  | "no-referrer" => NoReferrer
  | "no-referrer-when-downgrade" => NoReferrerWhenDowngrade
  | "same-origin" => SameOrigin
  | "origin" => Origin
  | "strict-origin" => StrictOrigin
  | "origin-when-cross-origin" => OriginWhenCrossOrigin
  | "strict-origin-when-cross-origin" => StrictOriginWhenCrossOrigin
  | "unsafe-url" => UnsafeUrl
  | e => raise(UnknownReferrerPolicy(e))
  }

type requestType =
  | None /* default? unknown? just empty string in spec */
  | Audio
  | Font
  | Image
  | Script
  | Style
  | Track
  | Video

exception UnknownRequestType(string)

let decodeRequestType = (requestType: string) =>
  switch requestType {
  | "" => None
  | "audio" => Audio
  | "font" => Font
  | "image" => Image
  | "script" => Script
  | "style" => Style
  | "track" => Track
  | "video" => Video
  | e => raise(UnknownRequestType(e))
  }

type requestDestination =
  | None /* default? unknown? just empty string in spec */
  | Document
  | Embed
  | Font
  | Image
  | Manifest
  | Media
  | Object
  | Report
  | Script
  | ServiceWorker
  | SharedWorker
  | Style
  | Worker
  | Xslt

exception UnknownRequestDestination(string)

let decodeRequestDestination = (requestDestination: string) =>
  switch requestDestination {
  | "" => None
  | "document" => Document
  | "embed" => Embed
  | "font" => Font
  | "image" => Image
  | "manifest" => Manifest
  | "media" => Media
  | "object" => Object
  | "report" => Report
  | "script" => Script
  | "serviceworker" => ServiceWorker
  | "sharedworker" => SharedWorker
  | "style" => Style
  | "worker" => Worker
  | "xslt" => Xslt
  | e => raise(UnknownRequestDestination(e))
  }

type requestMode =
  | Navigate
  | SameOrigin
  | NoCORS
  | CORS

let encodeRequestMode = (requestMode: requestMode) =>
  switch requestMode {
  | Navigate => "navigate"
  | SameOrigin => "same-origin"
  | NoCORS => "no-cors"
  | CORS => "cors"
  }

exception UnknownRequestMode(string)

let decodeRequestMode = (requestMode: string) =>
  switch requestMode {
  | "navigate" => Navigate
  | "same-origin" => SameOrigin
  | "no-cors" => NoCORS
  | "cors" => CORS
  | e => raise(UnknownRequestMode(e))
  }

type requestCredentials =
  | Omit
  | SameOrigin
  | Include

let encodeRequestCredentials = (requestCredentials: requestCredentials) =>
  switch requestCredentials {
  | Omit => "omit"
  | SameOrigin => "same-origin"
  | Include => "include"
  }

exception UnknownRequestCredentials(string)

let decodeRequestCredentials = (requestCredentials: string) =>
  switch requestCredentials {
  | "omit" => Omit
  | "same-origin" => SameOrigin
  | "include" => Include
  | e => raise(UnknownRequestCredentials(e))
  }

type requestCache =
  | Default
  | NoStore
  | Reload
  | NoCache
  | ForceCache
  | OnlyIfCached

let encodeRequestCache = (requestCache: requestCache) =>
  switch requestCache {
  | Default => "default"
  | NoStore => "no-store"
  | Reload => "reload"
  | NoCache => "no-cache"
  | ForceCache => "force-cache"
  | OnlyIfCached => "only-if-cached"
  }

exception UnknownRequestCache(string)

let decodeRequestCache = (requestCache: string) =>
  switch requestCache {
  | "default" => Default
  | "no-store" => NoStore
  | "reload" => Reload
  | "no-cache" => NoCache
  | "force-cache" => ForceCache
  | "only-if-cached" => OnlyIfCached
  | e => raise(UnknownRequestCache(e))
  }

type requestRedirect =
  | Follow
  | Error
  | Manual

let encodeRequestRedirect = (requestRedirect: requestRedirect) =>
  switch requestRedirect {
  | Follow => "follow"
  | Error => "error"
  | Manual => "manual"
  }

exception UnknownRequestRedirect(string)

let decodeRequestRedirect = (requestRedirect: string) =>
  switch requestRedirect {
  | "follow" => Follow
  | "error" => Error
  | "manual" => Manual
  | e => raise(UnknownRequestRedirect(e))
  }

module HeadersInit = {
  type t = headersInit

  external make: Js.t<{..}> => t = "%identity"
  external makeWithDict: Js.Dict.t<string> => t = "%identity"
  external makeWithArray: array<(string, string)> => t = "%identity"
}

module Headers = {
  type t = headers

  @new
  external make: t = "Headers"

  @new
  external makeWithInit: headersInit => t = "Headers"

  @send
  external append: (t, string, string) => unit = "append"

  @send
  external delete: (t, string) => unit = "delete"

  @send @return(nullable)
  external get: (t, string) => option<string> = "get"

  @send
  external has: (t, string) => bool = "has"

  @send
  external set: (t, string, string) => unit = "set"
}

module BodyInit = {
  type t = bodyInit

  external make: string => t = "%identity"
  external makeWithBlob: blob => t = "%identity"
  external makeWithBufferSource: bufferSource => t = "%identity"
  external makeWithFormData: formData => t = "%identity"
  external makeWithUrlSearchParams: urlSearchParams => t = "%identity"
}

module Body = {
  type t = body

  @get
  external body: t => readableStream = "body"

  @get
  external bodyUsed: t => bool = "bodyUsed"

  @send
  external arrayBuffer: t => Js.Promise.t<arrayBuffer> = "arrayBuffer"

  @send
  external blob: t => Js.Promise.t<blob> = "blob"

  @send
  external formData: t => Js.Promise.t<formData> = "formData"

  @send
  external json: t => Js.Promise.t<Js.Json.t> = "json"

  @send
  external text: t => Js.Promise.t<string> = "text"
}

module RequestInit = {
  type t = requestInit

  @obj
  external make: (
    @as("method") ~method_: string=?,
    ~headers: headersInit=?,
    ~body: bodyInit=?,
    ~referrer: string=?,
    ~referrerPolicy: string=?,
    ~mode: string=?,
    ~credentials: string=?,
    ~cache: string=?,
    ~redirect: string=?,
    ~integrity: string=?,
    ~keepalive: bool=?,
    ~signal: signal=?,
    unit,
  ) => t = ""

  let make = (
    ~method_: option<requestMethod>=?,
    ~headers: option<headersInit>=?,
    ~body: option<bodyInit>=?,
    ~referrer: option<string>=?,
    ~referrerPolicy: option<referrerPolicy>=?,
    ~mode: option<requestMode>=?,
    ~credentials: option<requestCredentials>=?,
    ~cache: option<requestCache>=?,
    ~redirect: option<requestRedirect>=?,
    ~integrity: option<string>=?,
    ~keepalive: option<bool>=?,
    ~signal: option<signal>=?,
    (),
  ) => {
    // ReScript syntax
    make(
      ~method_=?Option.map(method_, encodeRequestMethod),
      ~headers?,
      ~body?,
      ~referrer?,
      ~referrerPolicy=?Option.map(referrerPolicy, encodeReferrerPolicy),
      ~mode=?Option.map(mode, encodeRequestMode),
      ~credentials=?Option.map(credentials, encodeRequestCredentials),
      ~cache=?Option.map(cache, encodeRequestCache),
      ~redirect=?Option.map(redirect, encodeRequestRedirect),
      ~integrity?,
      ~keepalive?,
      ~signal?,
      (),
    )
  }
}

module Request = {
  type t = request

  @new
  external make: string => t = "Request"
  @new
  external makeWithInit: (string, requestInit) => t = "Request"
  @new
  external makeWithRequest: t => t = "Request"
  @new
  external makeWithRequestInit: (t, requestInit) => t = "Request"

  @get
  external method_: t => string = "method"
  let method_ = (self: t) => decodeRequestMethod(method_(self))

  @get
  external url: t => string = "url"

  @get
  external headers: t => headers = "headers"

  @get
  external type_: t => string = "type"
  let type_ = (self: t) => decodeRequestType(type_(self))

  @get
  external destination: t => string = "destination"
  let destination = (self: t) => decodeRequestDestination(destination(self))

  @get
  external referrer: t => string = "referrer"

  @get
  external referrerPolicy: t => string = "referrerPolicy"
  let referrerPolicy = (self: t) => decodeReferrerPolicy(referrerPolicy(self))

  @get
  external mode: t => string = "mode"
  let mode = (self: t) => decodeRequestMode(mode(self))

  @get
  external credentials: t => string = "credentials"
  let credentials = (self: t) => decodeRequestCredentials(credentials(self))

  @get
  external cache: t => string = "cache"
  let cache = (self: t) => decodeRequestCache(cache(self))

  @get
  external redirect: t => string = "redirect"
  let redirect = (self: t) => decodeRequestRedirect(redirect(self))

  @get
  external integrity: t => string = "integrity"

  @get
  external keepalive: t => bool = "keepalive"

  @get
  external signal: t => signal = "signal"

  /* Body Impl */
  @get
  external body: t => readableStream = "body"

  @get
  external bodyUsed: t => bool = "bodyUsed"

  @send
  external arrayBuffer: t => Js.Promise.t<arrayBuffer> = "arrayBuffer"

  @send
  external blob: t => Js.Promise.t<blob> = "blob"

  @send
  external formData: t => Js.Promise.t<formData> = "formData"

  @send
  external json: t => Js.Promise.t<Js.Json.t> = "json"

  @send
  external text: t => Js.Promise.t<string> = "text"
}

module Response = {
  type t = response

  @val
  external error: unit => t = "error"

  @val
  external redirect: string => t = "redirect"

  @val
  external redirectWithStatus: (string, int) => t = "redirect"

  @get
  external headers: t => headers = "headers"

  @get
  external ok: t => bool = "ok"

  @get
  external redirected: t => bool = "redirected"

  @get
  external status: t => int = "status"

  @get
  external statusText: t => string = "statusText"

  @get
  external _type: t => string = "_type"

  @get
  external url: t => string = "url"

  @send
  external clone: t => t = "clone"

  /* Body.Impl */

  @get
  external body: t => readableStream = "body"

  @get
  external bodyUsed: t => bool = "bodyUsed"

  @send
  external arrayBuffer: t => Js.Promise.t<arrayBuffer> = "arrayBuffer"

  @send
  external blob: t => Js.Promise.t<blob> = "blob"

  @send
  external formData: t => Js.Promise.t<formData> = "formData"

  @send
  external json: t => Js.Promise.t<Js.Json.t> = "json"

  @send
  external text: t => Js.Promise.t<string> = "text"
}

@val
external fetch: string => Js.Promise.t<response> = "fetch"

@val
external fetchWithInit: (string, requestInit) => Js.Promise.t<response> = "fetch"

@val
external fetchWithRequest: request => Js.Promise.t<response> = "fetch"

@val
external fetchWithRequestInit: (request, requestInit) => Js.Promise.t<response> = "fetch"
