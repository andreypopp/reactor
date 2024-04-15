console.log("react_browser_runtime.js: starting");

// import this to be defined before we import React/ReactDOM
window.__webpack_require__ = (id) => {
  let component = window.__exported_components[id];
  console.log('window.__webpack_require__', id, component);
  if (component == null)
    throw new Error(`unable to resolve client component "${id}"`);
  return {__esModule: true, default: component};
};

let React = require('react');
let ReactDOM = require('react-dom/client');
let ReactServerDOM = require("react-server-dom-webpack/client");

function callServer(id, args) {
  throw new Error(`callServer(${id}, ...): not supported yet`);
}

function Page({loading}) {
  let tree = React.use(loading);
  return tree;
}

let loading = null;
let loadingController = null;
let root = null;

function loadPage(path) {
  React.startTransition(() => {
    if (loadingController != null) {
      loadingController.abort();
    }
    loadingController = new AbortController();
    loading = ReactServerDOM.createFromFetch(
      fetch(path, { 
        method: "GET", 
        headers: {Accept: 'application/react.component'},
        signal: loadingController.signal,
      }),
      { callServer, }
    );
    if (root === null)
      root = ReactDOM.createRoot(document);
    root.render(
      <React.StrictMode>
        <Page loading={loading} />
      </React.StrictMode>
    );
    if (window.location.pathname !== path) {
      window.history.pushState({}, null, path);
    }
  });
}

window.React_of_caml_navigate = loadPage;

function main() {
  if (window.React_of_caml_ssr) {
    loading = ReactServerDOM.createFromReadableStream(
      window.React_of_caml_ssr.stream,
      { callServer, }
    );
    React.startTransition(() => {
      root = ReactDOM.hydrateRoot(document, 
        <React.StrictMode>
          <Page loading={loading} />
        </React.StrictMode>
      );
    });
  } else {
    loadPage(window.location.pathname);
  }
  window.addEventListener("popstate", (event) => {
    loadPage(window.location.pathname);
  });
}

main();
