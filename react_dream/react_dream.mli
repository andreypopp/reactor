(** Serve React applications with Dream. 

    {1 React Dream}

    {2 Example}

    A minimal example:

    {[
      let app _req =
        jsx.div [| textf "HELLO, WORLD!" |]

      let () =
        let links = [ "/static/bundle.css" ] in
        let scripts = [ "/static/bundle.js" ] in
        Dream.run
        @@ Dream.router
            [
              Dream.get "/static/**" "./static";
              Dream.get "/" (React_dream.render ~links ~scripts app);
            ]
    ]}

 *)

open React_server

(** {2 Reference} *)

val render :
  ?enable_ssr:bool ->
  ?scripts:string list ->
  ?links:string list ->
  (Dream.request -> React.element) ->
  Dream.handler
(** [render ~enable_ssr ~scripts ~links app] is a [Dream.handler] which serves
    [app], where:

    - [?enable_ssr] enables or disables Server Side Rendering (SSR). It is
    enabled by default.
    - [?scripts] and [?links] arguments allow to inject [<script>] and
    [<link rel="stylesheet">] tags into initial HTML skeleton.
 *)
