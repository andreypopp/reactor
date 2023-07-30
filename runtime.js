import React from 'react';
import ReactDOM from 'react-dom/client';
import { createFromFetch, } from "react-server-dom-webpack/client";

function callServer(id, args) {
  throw new Error(`callServer(${id}, ...): not supported yet`);
}

/** Just an example client side component. */
function Time(props) {
  let [time, setTime] = React.useState(null);
  React.useEffect(() => {
    setTime(new Date());
    let id = setInterval(() => setTime(t => new Date()), 1000);
    return () => clearInterval(id);
  }, []);
  return <div>{props.label}: {String(time)}</div>;
}

/** A registry of client side components. */
let ClientComponents = { Time, }

window.__webpack_require__ = (id) => {
  let component = ClientComponents[id];
  return {__esModule: true, default: component};
};

function createReactServerComponentWrapper(path) {
  // TODO: need a way to refetch
  // Start fetching RSC tree
  let loading = createFromFetch(
    fetch(path, { method: "GET", headers: {Accept: 'application/react.component'} }),
    { callServer, }
  );
  // This renders RSC
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
