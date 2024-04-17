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

- `ppx_router` - typed routes for Dream, WIP
