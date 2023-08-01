import React from 'react';
import ReactDOM from 'react-dom/client';
import { createFromFetch, } from "react-server-dom-webpack/client";

function callServer(id, args) {
  throw new Error(`callServer(${id}, ...): not supported yet`);
}

window.__webpack_require__ = (id) => {
  let component = window.__exported_components[id];
  if (component == null)
    throw new Error(`unable to resolve client component "${id}"`);
  return {__esModule: true, default: component};
};

function createReactServerComponentWrapper(path) {
  // TODO: need a way to refetch
  let loading = createFromFetch(
    fetch(path, { 
      method: "GET", 
      headers: {Accept: 'application/react.component'} 
    }),
    { callServer, }
  );
  return function Page() {
    return React.use(loading);
  }
}

async function main() {
  let Page = createReactServerComponentWrapper(window.location.pathname);
  let root = ReactDOM.createRoot(document.getElementById('root'));
  root.render(<Page />);
}

window.onload = () => main().catch(exn => console.error(exn));
