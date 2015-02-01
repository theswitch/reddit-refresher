open Core.Std
open Async.Std

(** create the uri for a thread given an id36 *)
let uri_of_thread thread limit =
  let url = Printf.sprintf "http://www.reddit.com/comments/%s.json" thread in
  let uri = Uri.of_string url in
  Uri.add_query_params' uri ["limit", Int.to_string limit; "sort", "new"; "depth", "2"]


(** retrieve the json for the given uri as a string *)
let get_thread_json uri =
  Cohttp_async.Client.get uri
    >>= fun (_, body) ->
      Cohttp_async.Body.to_string body
    >>| Yojson.Basic.from_string

(** extract and print the comments from response json *)
let get_comments j =
  let open Yojson.Basic.Util in
  let rec print_comments level j =
    j
    |> member "data"
    |> member "children" (* get list of children *)
    |> to_list
    (* make sure we only get comments *)
    |> List.filter ~f:(fun j -> member "kind" j |> to_string = "t1")
    (* process newest last *)
    |> List.rev
    (* process each comment; print the comment and its replies *)
    |> List.iter ~f:begin fun j ->
        let data = member "data" j in
        let author = member "author" data |> to_string
        and body   = member "body" data |> to_string
        in
        let sep = String.init (String.length author) (Fn.const '-')
        and indent = String.init (2 * level) (Fn.const ' ') in
        printf "%s%s\n%s%s\n\n%s%s\n\n" indent author indent sep indent body;
        match member "replies" data with
        | `Assoc _ as j -> print_comments (level + 1) j
        | _ -> ()
      end
  in
  index 1 j |> print_comments 0

let () =
  Command.async_basic
    ~summary:"Reddit thread refresher"
    Command.Spec.(
      empty
      +> anon ("id36" %: string)
      +> flag "-limit" (optional_with_default 5 int)
        ~doc:"number of comments to fetch"
    )
    (fun thread limit () ->
      uri_of_thread thread limit
      |> get_thread_json
      >>| get_comments)
  |> Command.run
