open Lwt
open Cohttp
open Cohttp_lwt_unix

let proxy_uri = ref ""
let direct = ref false
let tunnel = ref false
let uri = ref []

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
  ]

let main ?proxy ~uri () =
  let ctx = Client.custom_ctx ?proxy () in
  (match proxy with
  | Some (`CONNECT, _proxy) -> failwith "unimplemented"
  | Some (`GET, _) | _ -> Lwt.return_unit)
  >>= fun () ->
  Client.get ~ctx (Uri.of_string uri) >>= fun (resp, body) ->
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
