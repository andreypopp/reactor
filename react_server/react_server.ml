open! Import

type json = Yojson.Safe.t

module React = React
module React_browser = React_browser

let render_to_model = Render_to_model.render
let render_to_html = Render_to_html.render

module Html = Html
