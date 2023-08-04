(* Borrowed from https://github.com/dbuenzli/htmlit/blob/main/src/htmlit.ml

   Copyright (c) 2016 The htmlit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
*)

open ContainersLabels
open Monomorphic

type attrs = (string * [ `String of string | `Bool of bool ]) list

type t =
  | H_node of string * attrs * t list option
  | H_text of string
  | H_raw of string
  | H_splice of t list * string

let node name props children = H_node (name, props, children)
let text text = H_text text
let raw data = H_raw data
let splice ?(sep = "") xs = H_splice (xs, sep)

let add_escaped b s =
  let adds = Buffer.add_string in
  let len = String.length s in
  let max_idx = len - 1 in
  let flush b start i =
    if start < len then Buffer.add_substring b s start (i - start)
  in
  let rec loop start i =
    if i > max_idx then flush b start i
    else
      let next = i + 1 in
      match String.get s i with
      | '&' ->
          flush b start i;
          adds b "&amp;";
          loop next next
      | '<' ->
          flush b start i;
          adds b "&lt;";
          loop next next
      | '>' ->
          flush b start i;
          adds b "&gt;";
          loop next next
      | '\'' ->
          flush b start i;
          adds b "&apos;";
          loop next next
      | '\"' ->
          flush b start i;
          adds b "&quot;";
          loop next next
      | _ -> loop start next
  in
  loop 0 0

let rec write buf =
  let adds s = Buffer.add_string buf s in
  function
  | H_splice (xs, sep) ->
      let rec aux = function
        | [] -> ()
        | [ x ] -> write buf x
        | [ x; y ] ->
            write buf x;
            adds sep;
            write buf y
        | x :: xs ->
            write buf x;
            adds sep;
            aux xs
      in
      aux xs
  | H_raw data -> adds data
  | H_text text -> add_escaped buf text
  | H_node (name, props, children) -> (
      adds "<";
      adds name;
      let () =
        match props with
        | [] -> ()
        | attrs ->
            List.iter attrs ~f:(fun (name, value) ->
                adds " ";
                let name =
                  match name with "className" -> "class" | name -> name
                in
                match value with
                | `Bool false -> ()
                | `Bool true -> adds name
                | `String value ->
                    adds name;
                    adds "=\"";
                    add_escaped buf value;
                    adds "\"")
      in
      match children with
      | None -> adds " />"
      | Some children ->
          adds ">";
          List.iter children ~f:(write buf);
          adds "</";
          adds name;
          adds ">")

let to_string html =
  let buf = Buffer.create 1024 in
  write buf html;
  Buffer.contents buf