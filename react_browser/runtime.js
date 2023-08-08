import * as React from 'react';
import * as ReactDOM from 'react-dom/client';
import * as ReactServerDOM from "react-server-dom-webpack/client";

function callServer(id, args) {
  throw new Error(`callServer(${id}, ...): not supported yet`);
}

window.__webpack_require__ = (id) => {
  let component = window.__exported_components[id];
  if (component == null)
    throw new Error(`unable to resolve client component "${id}"`);
  return {__esModule: true, default: component};
};


function Page({loading}) {
  return React.use(loading);
}

async function main() {
  if (window.React_of_caml_ssr) {
    let loading = ReactServerDOM.createFromReadableStream(
      window.React_of_caml_ssr.stream,
      { callServer, }
    );
    let element = React.createElement(Page, {loading})
    React.startTransition(() => {
      ReactDOM.hydrateRoot(document, element)
    })
  } else {
    let loading = ReactServerDOM.createFromFetch(
      fetch(window.location.pathname, { 
        method: "GET", 
        headers: {Accept: 'application/react.component'} 
      }),
      { callServer, }
    );
    let root = ReactDOM.createRoot(document);
    root.render(React.createElement(Page, {loading}));
  }
}

main();
