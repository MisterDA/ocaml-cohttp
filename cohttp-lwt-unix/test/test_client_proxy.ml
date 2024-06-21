open Lwt
open Cohttp
open Cohttp_lwt_unix

let body ~proxy ~uri =
  let proxy = Uri.of_string proxy and uri = Uri.of_string uri in
  let ctx = Client.custom_ctx ~proxy () in
  Client.get ~ctx uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  let body = Lwt_main.run (body ~proxy:Sys.argv.(1) ~uri:Sys.argv.(2)) in
  print_endline ("Received body\n" ^ body)
