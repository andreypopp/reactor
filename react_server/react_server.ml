open ContainersLabels
open Monomorphic
open Printf
open Lwt.Infix

type json = Yojson.Safe.t
(* used to transfer props to/from browser *)

module React = struct
  type any_promise = Any_promise : 'a Lwt.t -> any_promise

  exception Suspend of any_promise
  (** React.use raises this when the promise is not resolved yet *)

  type element =
    | El_null : element
    | El_suspense : {
        children : children;
        fallback : children;
      }
        -> element
    | El_text : string -> element
    | El_html : string * html_props * children option -> element
    | El_thunk : (unit -> element) -> element
    | El_async_thunk : (unit -> element Lwt.t) -> element
    | El_client_thunk : {
        import_module : string;
        import_name : string;
        props : client_props;
        thunk : element;
      }
        -> element

  and children = element array
  and html_props = (string * [ `String of string | `Bool of bool ]) list
  and client_props = (string * [ json | `Element of element ]) list

  let null = El_null
  let text s = El_text s
  let textf fmt = ksprintf text fmt
  let thunk f = El_thunk f
  let async_thunk f = El_async_thunk f

  let suspense ?(fallback = [| null |]) children =
    El_suspense { children; fallback }

  let client_thunk ?(import_name = "") import_module props thunk =
    El_client_thunk { import_module; import_name; props; thunk }

  type html_element = ?className:string -> children -> element

  let html' tag_name : html_element =
   fun ?className children ->
    let props =
      [ "className", Option.map Html.s className ]
      |> List.filter_map ~f:(function
           | _, None -> None
           | n, Some v -> Some (n, v))
    in
    El_html (tag_name, props, Some children)

  let h = html'
  let html = html' "html"
  let body = html' "body"
  let head = html' "head"
  let title = html' "title"
  let div = html' "div"
  let a = html' "a"
  let span = html' "span"
  let li = html' "li"
  let ul = html' "ul"
  let ol = html' "ol"
  let h1 = html' "h1"
  let h2 = html' "h2"
  let h3 = html' "h3"
  let use_effect _thunk _deps = assert false
  let use_effect' _thunk _deps = assert false

  type 'a promise = 'a Lwt.t

  let use _promise = assert false
end

module React_browser = struct
  module Promise = struct
    type 'a promise = 'a Lwt.t

    let sleep secs = Lwt_unix.sleep secs
  end

  module React = struct
    include React

    let use_effect _thunk _deps = ()
    let use_effect' _thunk _deps = ()

    type 'a promise = 'a Lwt.t

    let use promise =
      match Lwt.state promise with
      | Return v -> v
      | Sleep -> raise (Suspend (Any_promise promise))
      | Fail exn -> raise exn
  end
end

module Render_to_model : sig
  type model = json

  val node :
    name:string -> props:(string * model) list -> model option -> model

  val ref : import_module:string -> import_name:string -> model
  val null : model
  val text : string -> model
  val list : model list -> model
  val suspense : model -> model
  val suspense_placeholder : int -> model

  type chunk = C_tree of model | C_ref of model

  val chunk_to_string : int * chunk -> string

  val render : React.element -> (string -> unit Lwt.t) -> unit Lwt.t
  (** Render React elements into a serialized model. *)
end = struct
  type model = json

  let text text = `String text
  let null = `Null
  let list xs = `List xs

  let node ~name ~props children : model =
    let props =
      match children with
      | None -> props
      | Some children -> ("children", children) :: props
    in
    `List [ `String "$"; `String name; `Null; `Assoc props ]

  let suspense children =
    node ~name:"$Sreact.suspense" ~props:[] (Some children)

  let suspense_placeholder idx =
    node ~name:"$Sreact.suspense" ~props:[]
      (Some (`String (sprintf "$L%i" idx)))

  let ref ~import_module ~import_name =
    `Assoc
      [
        "id", `String import_module;
        "name", `String import_name;
        "chunks", `List [];
        "async", `Bool false;
      ]

  type chunk = C_tree of model | C_ref of model

  let chunk_to_string = function
    | idx, C_ref ref ->
        let buf = Buffer.create 256 in
        Buffer.add_string buf (sprintf "%x:I" idx);
        Yojson.Safe.write_json buf ref;
        Buffer.add_char buf '\n';
        Buffer.contents buf
    | idx, C_tree model ->
        let buf = Buffer.create (4 * 1024) in
        Buffer.add_string buf (sprintf "%x:" idx);
        Yojson.Safe.write_json buf model;
        Buffer.add_char buf '\n';
        Buffer.contents buf

  type ctx = {
    mutable idx : int;
    mutable waiting : int;
    push : string option -> unit;
  }

  let use_idx ctx =
    ctx.idx <- ctx.idx + 1;
    ctx.idx

  let push ctx chunk = ctx.push (Some (chunk_to_string chunk))

  let rec to_model ctx idx el =
    let rec to_model' : React.element -> model = function
      | React.El_null -> `Null
      | El_text s -> `String s
      | El_html (name, props, children) ->
          let children =
            Option.map
              (fun children ->
                Array.to_list children |> List.map ~f:to_model' |> list)
              children
          in
          node ~name ~props:(props :> (string * json) list) children
      | El_suspense { children; fallback = _ } ->
          suspense
            (Array.to_list children |> List.map ~f:to_model' |> list)
      | El_thunk f -> to_model' (f ())
      | El_async_thunk f -> (
          let tree = f () in
          match Lwt.state tree with
          | Lwt.Return tree -> to_model' tree
          | Lwt.Fail exn -> raise exn
          | Lwt.Sleep ->
              let idx = use_idx ctx in
              ctx.waiting <- ctx.waiting + 1;
              Lwt.async (fun () -> Lwt.map (to_model ctx idx) tree);
              `String (sprintf "$L%i" idx))
      | El_client_thunk { import_module; import_name; props; thunk = _ }
        ->
          let idx = use_idx ctx in
          let ref = ref ~import_module ~import_name in
          push ctx (idx, C_ref ref);
          let props =
            List.map props ~f:(function
              | name, `Element element -> name, to_model' element
              | name, (#json as json) -> name, (json :> json))
          in
          node ~name:(sprintf "$%i" idx) ~props None
    in
    push ctx (idx, C_tree (to_model' el));
    if ctx.waiting = 0 then ctx.push None

  let render el on_chunk =
    let rendering, push = Lwt_stream.create () in
    let ctx = { push; waiting = 0; idx = 0 } in
    to_model ctx ctx.idx el;
    Lwt_stream.iter_s on_chunk rendering
end

module Render_to_html = struct
  type ctx = {
    mutable idx : int;
    mutable waiting : int;
    mutable instruction_RC_sent : bool;
    push : (Html.t * (int * Render_to_model.chunk)) option -> unit;
  }

  let use_idx ctx =
    ctx.idx <- ctx.idx + 1;
    ctx.idx

  type mode = Mode_server | Mode_client

  let instruction_RC =
    {|$RC=function(b,c,e){c=document.getElementById(c);c.parentNode.removeChild(c);var a=document.getElementById(b);if(a){b=a.previousSibling;if(e)b.data="$!",a.setAttribute("data-dgst",e);else{e=b.parentNode;a=b.nextSibling;var f=0;do{if(a&&8===a.nodeType){var d=a.data;if("/$"===d)if(0===f)break;else f--;else"$"!==d&&"$?"!==d&&"$!"!==d||f++}d=a.nextSibling;e.removeChild(a);a=d}while(a);for(;c.firstChild;)e.insertBefore(c.firstChild,a);b.data="$"}b._reactRetry&&b._reactRetry()}};|}

  let emit_chunk ctx idx html =
    let html =
      [
        Html.node "div"
          [ "hidden", `Bool true; "id", `String (sprintf "S:%i" idx) ]
          (Some [ html ]);
        Html.rawf {|<script>$RC("B:%i", "S:%i")</script>|} idx idx;
      ]
    in
    let html =
      match ctx.instruction_RC_sent with
      | true -> html
      | false ->
          ctx.instruction_RC_sent <- true;
          Html.rawf "<script>%s</script>" instruction_RC :: html
    in
    Html.splice html ~sep:"\n"

  let emit_placeholder idx =
    Html.rawf {|<!--$?--><template id="B:%i"></template><!--/$-->|} idx

  let to_html ctx mode el =
    let rec to_html' this mode = function
      | React.El_null -> Lwt.return (Html.text "", Render_to_model.null)
      | El_text s -> Lwt.return (Html.text s, Render_to_model.text s)
      | El_html (name, props, children) ->
          (match children with
          | None -> Lwt.return_none
          | Some children ->
              to_html_many this mode children >|= Option.some)
          >|= fun children ->
          let html, children =
            match children with
            | None -> None, None
            | Some (html, children) -> Some [ html ], Some children
          in
          ( Html.node name props html,
            Render_to_model.node ~name
              ~props:(props :> (string * json) list)
              children )
      | El_suspense { children; fallback = _ } -> (
          match mode with
          | Mode_client -> (
              match Array.length children with
              | 0 ->
                  Lwt.return
                    ( Html.raw {|<!--$?--><!--/$-->|},
                      Render_to_model.suspense Render_to_model.null )
              | _ ->
                  let restart () = to_html_many this mode children in
                  let rec wait (React.Any_promise promise) =
                    promise >>= fun _ ->
                    Lwt.catch restart (function
                      | React.Suspend any_promise -> wait any_promise
                      | exn -> Lwt.fail exn)
                    >>= fun v ->
                    this >|= fun () -> v
                  in
                  Lwt.catch restart (function
                    | React.Suspend any_promise ->
                        let idx = use_idx ctx in
                        ctx.waiting <- ctx.waiting + 1;
                        Lwt.async (fun () ->
                            wait any_promise >|= fun (html, model) ->
                            let html = emit_chunk ctx idx html in
                            ctx.push
                              (Some
                                 ( html,
                                   (idx, Render_to_model.C_tree model) ));
                            ctx.waiting <- ctx.waiting - 1;
                            if ctx.waiting = 0 then ctx.push None);
                        Lwt.return
                          ( emit_placeholder idx,
                            Render_to_model.suspense_placeholder idx )
                    | exn -> Lwt.fail exn))
          | Mode_server -> (
              let promise = to_html_many this mode children in
              match Lwt.state promise with
              | Sleep ->
                  let idx = use_idx ctx in
                  ctx.waiting <- ctx.waiting + 1;
                  Lwt.async (fun () ->
                      promise >>= fun (html, model) ->
                      this >|= fun () ->
                      let html = emit_chunk ctx idx html in
                      ctx.push
                        (Some (html, (idx, Render_to_model.C_tree model)));
                      ctx.waiting <- ctx.waiting - 1;
                      if ctx.waiting = 0 then ctx.push None);
                  Lwt.return
                    ( emit_placeholder idx,
                      Render_to_model.suspense_placeholder idx )
              | Return (html, model) ->
                  Lwt.return (html, Render_to_model.suspense model)
              | Fail exn -> Lwt.fail exn))
      | El_thunk f -> to_html' this mode (f ())
      | El_async_thunk f -> (
          match mode with
          | Mode_client -> failwith "async component in client mode"
          | Mode_server -> f () >>= to_html' this mode)
      | El_client_thunk { import_module; import_name; props; thunk } ->
          to_html' this Mode_client thunk >>= fun (html, _model) ->
          let idx = use_idx ctx in
          let ref = Render_to_model.ref ~import_module ~import_name in
          ctx.push (Some (Html.text "", (idx, C_ref ref)));
          Lwt_list.map_p (json_model_to_model this mode) props
          >|= fun props ->
          let model =
            Render_to_model.node ~name:(sprintf "$%i" idx) ~props None
          in
          html, model
    and to_html_many this mode els =
      Array.to_list els
      |> Lwt_list.map_p (to_html' this mode)
      >|= List.split
      >|= fun (htmls, model) ->
      Html.splice htmls ~sep:"<!-- -->", Render_to_model.list model
    and json_model_to_model this mode (name, jsony) =
      match jsony with
      | `Element element ->
          to_html' this mode element >|= fun (_html, model) -> name, model
      | #json as jsony -> Lwt.return (name, (jsony :> json))
    in
    let this, ready = Lwt.wait () in
    let idx = ctx.idx in
    to_html' this mode el >|= fun (html, json) ->
    ctx.push (Some (html, (idx, Render_to_model.C_tree json)));
    Lwt.wakeup ready ();
    ctx.waiting <- ctx.waiting - 1;
    if ctx.waiting = 0 then ctx.push None

  let render el on_chunk =
    let rendering, push = Lwt_stream.create () in
    let ctx =
      { push; waiting = 1; idx = 0; instruction_RC_sent = false }
    in
    to_html ctx Mode_server el >>= fun () ->
    Lwt_stream.iter_s on_chunk rendering
end

module Model_bootstrap = struct
  let emit' stream fmt =
    ksprintf
      (fun js ->
        Dream.write stream
          Html.(node "script" [] (Some [ rawf "{%s}" js ]) |> to_string))
      fmt

  let runtime =
    String.trim
      {|
let enc = new TextEncoder();
let SSR = (window.SSR = {});
SSR.push = (data) => SSR._c.enqueue(enc.encode(data));
SSR.close = () => SSR._c.close();
SSR.stream = new ReadableStream({ start(c) { SSR._c = c; } });|}

  let emit_runtime stream = emit' stream "%s" runtime

  let emit_chunk stream chunk =
    emit' stream "window.SSR.push(%S)"
      (Render_to_model.chunk_to_string chunk)

  let emit_close stream = emit' stream "window.SSR.close()"
end

let html_prelude ~scripts ~links =
  let open Html in
  let make_link href =
    node "link" [ "href", s href; "rel", s "stylesheet" ] None
  in
  let make_script src = node "script" [ "src", s src ] (Some []) in
  splice ~sep:"\n"
    [
      List.map links ~f:make_link |> Html.splice ~sep:"\n";
      List.map scripts ~f:make_script |> Html.splice ~sep:"\n";
    ]

let render_to_html ?(scripts = []) ?(links = []) =
  let html_prelude = html_prelude ~links ~scripts |> Html.to_string in
  fun f : Dream.handler ->
    fun req ->
     match Dream.header req "accept" with
     | Some "application/react.component" ->
         Dream.stream (fun stream ->
             Render_to_model.render (f req) (fun data ->
                 Dream.write stream data >>= fun () -> Dream.flush stream))
     | _ ->
         Dream.stream (fun stream ->
             Model_bootstrap.emit_runtime stream >>= fun () ->
             Dream.write stream html_prelude >>= fun () ->
             Render_to_html.render (f req) (fun (html, chunk) ->
                 Model_bootstrap.emit_chunk stream chunk >>= fun () ->
                 Dream.write stream (Html.to_string html) >>= fun () ->
                 Dream.flush stream)
             >>= fun () -> Model_bootstrap.emit_close stream)

let render ?(scripts = []) ?(links = []) =
  let html_prelude = html_prelude ~links ~scripts |> Html.to_string in
  fun f : Dream.handler ->
    fun req ->
     match Dream.header req "accept" with
     | Some "application/react.component" ->
         Dream.stream (fun stream ->
             Render_to_model.render (f req) (fun data ->
                 Dream.write stream data >>= fun () -> Dream.flush stream))
     | _ -> Dream.html html_prelude
