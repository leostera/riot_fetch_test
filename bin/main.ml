open Riot

let (let*) = Result.bind

let fetch url = 
  let url = Uri.of_string url in
  let* conn = Blink.connect url in
  let req = Http.Request.make "/" in
  let* conn = Blink.request conn req () in
  let* _conn, frames = Blink.stream conn in
  match frames with
  | [ `Status _status; `Headers _headers; _ ] -> Ok ()
  | _ -> Error `something_went_wrong

let () = 
  Riot.run @@ fun () ->
    Logger.set_log_level (Some Info);
    let make_calls batch_id =
      let tasks = List.init 50 (fun _ -> Task.async (fun () -> fetch "https://google.com")) in
      List.map Task.await tasks |> ignore;
      Logger.info (fun f -> f "[total=%d] sent 50 requests" (batch_id*50))
    in
    List.init 300 make_calls |> ignore
