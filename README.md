# reactor - React OCaml Runtime

**WARNING: EXPERIMENTAL, DO NOT USE**

This repository hosts the following packages:

- `react` - a native OCaml implementation of the server side component for the React.js library.

  More specifically it implements Server Side Rendering (SSR) and React Server
  Components (RSC) which is interoperable with React.js, supporting Suspense.

- `react_dream` - a thin layer of code to integrate `react` with `dream`.

- `remote` - a data fetching library with request caching and deduplication,
  which works with SSR (fetched payload is being to transfered to client so no
  it doesn't have to be refetched).

- `realm` - a compatibility layer for code written to be compiled both native
  OCaml toolchain and Melange.

## Run example apps

The `example/` directory contains a few example apps:

- `make build react-example-server-only` showcases React Server Components
  rendered on server to HTML and no hydration on client is performed (means no
  JS is needed). One can consier React being a fancy template engine in this
  case.

- `make build react-example-rsc` showcases React Server Components which render
  on server to JSON model and then on client the JSON model is rendered to
  HTML, invoking the client components.

- `make build react-example-rsc-ssr` showcases React Server Components which
  render on server to HTML, emitting JSON model along. On client the JSON model
  is being used to hydrate the server rendered HTML.

All the examples feature typesafe routing with `ppx_deriving_router` and data
fetching with `remote`.
