let rec run out =
  print_string ">>= ";
  output_string out (read_line ());
  output_string out "\n";
  flush out;
  run out

let _ =
  let out = Sys.argv.(1) |> open_out in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
      close_out_noerr out;
      exit 0 ));
  run out
