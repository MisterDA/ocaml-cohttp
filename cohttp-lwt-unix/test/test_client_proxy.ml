open Lwt
open Cohttp
open Cohttp_lwt_unix

let () =
  if not @@ Debug.debug_active () then (
    Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ();
    Logs.set_level ~all:true (Some Logs.Debug);
    Logs.set_reporter Debug.default_reporter)

let proxy_uri = ref ""
let direct = ref false
let tunnel = ref false
let uri = ref []
let proxy_auth = ref ""

let usage_msg =
  {|Usage: test_client_proxy -proxy <uri> [-direct | -tunnel] <resource>
Examples:
$ test_client_proxy -proxy http://localhost:8080 -direct http://example.com
$ test_client_proxy -proxy https://localhost:8080 -tunnel https://example.com
Options:|}

let anon_fun args = uri := !uri @ [ args ]

let speclist =
  [
    ("-proxy", Arg.Set_string proxy_uri, "<uri>  Proxy uri");
    ("-direct", Arg.Set direct, " Direct proxy (GET)");
    ("-tunnel", Arg.Set tunnel, " Tunnel proxy (CONNECT)");
    ("-proxyauth", Arg.Set_string proxy_auth, " Proxy authentication");
  ]

let rec http_get_and_follow ~ctx ~max_redirects uri =
  let open Lwt.Syntax in
  let* ans = Cohttp_lwt_unix.Client.get ~ctx uri in
  follow_redirect ~ctx ~max_redirects uri ans

and follow_redirect ~ctx ~max_redirects request_uri (response, body) =
  let open Lwt.Syntax in
  let status = Http.Response.status response in
  (* The unconsumed body would otherwise leak memory *)
  let* () =
    if status <> `OK then Cohttp_lwt.Body.drain_body body else Lwt.return_unit
  in
  match status with
  | `OK -> Lwt.return (response, body)
  | `Permanent_redirect | `Moved_permanently ->
      handle_redirect ~ctx ~permanent:true ~max_redirects request_uri response
  | `Found | `Temporary_redirect ->
      handle_redirect ~ctx ~permanent:false ~max_redirects request_uri response
  | `Not_found | `Gone -> Lwt.fail_with "Not found"
  | status ->
      Lwt.fail_with
        (Printf.sprintf "Unhandled status: %s"
           (Cohttp.Code.string_of_status status))

and handle_redirect ~ctx ~permanent ~max_redirects request_uri response =
  if max_redirects <= 0 then Lwt.fail_with "Too many redirects"
  else
    let headers = Http.Response.headers response in
    let location = Http.Header.get headers "location" in
    match location with
    | None -> Lwt.fail_with "Redirection without Location header"
    | Some url ->
        let open Lwt.Syntax in
        let uri = Uri.of_string url in
        let* () =
          if permanent then
            Logs_lwt.warn (fun m ->
                m "Permanent redirection from %s to %s"
                  (Uri.to_string request_uri)
                  url)
          else Lwt.return_unit
        in
        http_get_and_follow ~ctx uri ~max_redirects:(max_redirects - 1)

let main ?proxy ~uri () =
  let ctx = Client.custom_ctx ?proxy () in
  (match proxy with
  | Some (`CONNECT, _) ->
      let headers =
        match !proxy_auth with
        | "" -> None
        | proxy_auth ->
            Some
              (Header.of_list
                 [
                   ( "Proxy-Authorization",
                     "basic " ^ Base64.encode_exn proxy_auth );
                 ])
      in
      Client.connect ?headers ~ctx (Uri.of_string uri) >>= fun resp ->
      let code = resp |> Response.status |> Code.code_of_status in
      if not (Code.is_success code) then (
        Printf.eprintf "Could not setup tunnel. Response code: %d\n" code;
        exit 1);
      Lwt.return_unit
  | Some (`GET, _) | _ -> Lwt.return_unit)
  >>= fun () ->
  http_get_and_follow ~ctx ~max_redirects:2 (Uri.of_string uri)
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  Arg.parse speclist anon_fun usage_msg;
  if List.length !uri <> 1 then (
    prerr_endline "Expected a single resource uri.";
    prerr_endline usage_msg;
    exit 1);
  if !direct && !tunnel then (
    prerr_endline "Select either direct or tunnel proxy.";
    prerr_endline usage_msg;
    exit 1);
  let proxy =
    if !proxy_uri <> "" && (not !direct) && not !tunnel then (
      prerr_endline
        "Need to specify whether the proxy is direct (GET) or tunneling \
         (CONNECT).";
      exit 1)
    else if !proxy_uri = "" || ((not !direct) && not !tunnel) then (
      prerr_endline "Warning: not using a proxy server.";
      None)
    else if !direct then Some (`GET, Uri.of_string !proxy_uri)
    else Some (`CONNECT, Uri.of_string !proxy_uri)
  in
  let body = Lwt_main.run (main ?proxy ~uri:(List.hd !uri) ()) in
  print_endline ("Received body\n" ^ body)
