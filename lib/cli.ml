open Cmdliner
open Run

let version = "v1.0.0"

let input_file =
  let doc = "Input file to execute" in
  Arg.(value & opt (some string) None & info [ "i"; "input" ] ~docv:"FILE" ~doc)

let run_cmd input_file =
  match input_file with
  | Some file -> (
      try
        let content = read_file file in
        run_program content
      with Sys_error msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1)
  | None -> Run.repl ()

let run_t = Term.(const run_cmd $ input_file)

let cmd =
  let doc =
    "A cool interpreted language made in coolest programming language"
  in
  let man =
    [
      `S Manpage.s_description;
      `P "Zin is a cool interpreted programming language implemented in OCaml.";
      `P "When run without arguments, it starts an interactive REPL.";
      `P "Use -i FILE to execute a file containing Zin code.";
      `S Manpage.s_examples;
      `P "Start REPL:";
      `P "  $(tname)";
      `P "Execute a file:";
      `P "  $(tname) -i program.zin";
      `S Manpage.s_bugs;
      `P "Report bugs to the project repository.";
    ]
  in
  let info = Cmd.info "zin" ~version ~doc ~man in
  Cmd.v info run_t

let main () = exit (Cmd.eval cmd)
