open! Import

type json = Yojson.Basic.t

module React = React_model

let render_to_model = Render_to_model.render

type html_rendering = Render_to_html.html_rendering =
  | Html_rendering_done of { html : Htmlgen.t }
  | Html_rendering_async of {
      html_shell : Htmlgen.t;
      html_iter : (Htmlgen.t -> unit Lwt.t) -> unit Lwt.t;
    }

let render_to_html = Render_to_html.render

type browser_only
