// import this to be defined before we import React/ReactDOM
window.__webpack_require__ = (id) => {
  let component = window.__exported_components[id];
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

class HTTPChunkedParser {
  constructor() {
    this.remainder = new Uint8Array();
    this.textDecoder = new TextDecoder();
  }

  transform(chunk, controller) {
    let data = new Uint8Array(this.remainder.length + chunk.length);
    data.set(this.remainder);
    data.set(chunk, this.remainder.length);

    let offset = 0;
    while (offset < data.length) {
      if (offset + 2 > data.length) break; // need at least 2 bytes

      // find the end of the length line (CRLF)
      let endOfLine = data.subarray(offset).indexOf(0x0A); // 0x0A is newline in ASCII
      if (endOfLine === -1) break; // need more data
      let lengthS = data.subarray(offset, offset + endOfLine);
      let length = parseInt(this.textDecoder.decode(lengthS).trim(), 16);

      if (isNaN(length)) {
        console.error("Invalid chunk length");
        controller.error("Invalid chunk length");
        return;
      }

      // Calculate start and end of the data chunk
      let start = offset + endOfLine + 1;
      let end = start + length;
      if (end > data.length) break; // need more data

      controller.enqueue(data.subarray(start, end)); // extract the data chunk and enqueue it
      offset = end + 2; // move the offset past this chunk and the following CRLF
    }
    this.remainder = data.subarray(offset);
  }

  flush(controller) {
    if (this.remainder.length > 0) {
      // Handle any remaining data that was not a complete chunk
      console.error("Incomplete final chunk");
      controller.error("Incomplete final chunk");
    }
  }
}

function fetchRSC(path) {
  return fetch(path, {
    method: 'GET',
    headers: {Accept: 'application/react.component'},
    signal: loadingController.signal,
  }).then(response => {
    let p = new HTTPChunkedParser();
    let t = new TransformStream(p);
    response.body.pipeThrough(t);
    response = {body: t.readable};
    return response;
  });
}

function loadPage(path) {
  React.startTransition(() => {
    if (loadingController != null) {
      loadingController.abort();
    }
    loadingController = new AbortController();
    loading = ReactServerDOM.createFromFetch(fetchRSC(path), { callServer });
    if (root === null)
      root = ReactDOM.createRoot(document);
    root.render(
      <React.StrictMode>
        <Page loading={loading} />
      </React.StrictMode>
    );
    let {pathname, search} = window.location;
    if (pathname + search !== path)
      window.history.pushState({}, null, path);
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
    let {pathname, search} = window.location;
    loadPage(pathname + search);
  }
  window.addEventListener("popstate", (event) => {
    let {pathname, search} = window.location;
    loadPage(pathname + search);
  });
}

main();
