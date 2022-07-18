(* ----- Watcher Code Start ------------------------------------------------ *)
let string_of_event event =
  (*
     알쏭달쏭한 디스트럭쳐링이다.
     변수를 꺼낼 때 Fsevents_lwt.path라는식으로 꺼냈는데
     Fsevents_lwt파일을 보면 path라는건 없다
     이 path가 event.path를 의미한다는게 자동으로 추론되었고
     그렇기 때문에 event에 대한 어노테이션 없이 
     event가 Fsevents_lwt.event 임이 추론되었다.
     꺼낼때는 Fsevents_lwt.path라고 했지만, 쓸때는 그냥 path라고 쓴다.
  *)
  let { Fsevents_lwt.path; flags; _ } = event in
  path ^ "\n" ^ (flags |> Fsevents.EventFlags.to_string)

let make_watcher paths =
  paths |> Fsevents_lwt.create 0. Fsevents.CreateFlags.detailed_interactive

let make_stream watcher = watcher |> Fsevents_lwt.stream
let make_event_stream watcher = watcher |> Fsevents_lwt.event_stream
let print_event event = event |> string_of_event |> Lwt_io.printf "%s%!\n"
let print_events stream = stream |> Lwt_stream.iter_s print_event

let timer =
  let open Lwt in
  let rec print_time () =
    let { Unix.tm_hour; tm_min; tm_sec; _ } =
      Unix.(localtime (gettimeofday ()))
    in
    (* >>= ?? 이건 또 뭐야 *)
    Lwt_io.printf "%02d:%02d:%02d%!\n" tm_hour tm_min tm_sec >>= fun () ->
    Lwt_unix.sleep 10. >>= fun () -> print_time ()
  in
  print_time ()

let run_fs_watcher filepath =
  let watcher = [ filepath ] |> make_watcher in

  Lwt.(
    async (fun () ->
        Cf_lwt.RunLoop.run_thread (fun runloop ->
            let event_stream = watcher |> make_event_stream in

            Fsevents.schedule_with_run_loop event_stream runloop
              Cf.RunLoop.Mode.Default;
            if not (Fsevents.start event_stream) then
              prerr_endline "failed starting FSEvents stream")
        >>= fun _ -> Lwt.return_unit));

  Lwt.async (fun () -> watcher |> make_stream |> print_events);

  Lwt_main.run timer

(* ----- Watcher Code End -------------------------------------------------- *)

(* ----- Cli Code Start ---------------------------------------------------- *)

let filepath_param =
  let open Core.Command.Param in
  anon ("filepath" %: string)

let command =
  Core.Command.basic ~summary:"File Watcher Based on Darwin FsEvents"
    (Core.Command.Param.map filepath_param ~f:(fun filepath () ->
         filepath |> run_fs_watcher))

(* ----- Cli Code End ------------------------------------------------------ *)

let () = Command_unix.run command ~version:"0.1"
