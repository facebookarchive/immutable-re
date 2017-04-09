(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Topkg

let () =
  let cmd c os files =
    let ocamlbuild = Conf.tool "rebuild" os in
    OS.Cmd.run @@ Cmd.(ocamlbuild % "-use-ocamlfind"
                                  %% (v "-I" % "src/core")
                                  %% (v "-I" % "src/queues")
                                  %% (v "-I" % "src/indexed")
                                  %% (v "-I" % "src/utils")
                                  %% (v "-I" % "src/maps")
                                  %% (v "-I" % "src/sets")
                                  %% of_list files)
  in
  let build = Pkg.build ~cmd () in
  Pkg.describe "immutable" ~build ~change_logs:[] ~licenses:[] ~readmes:[] @@ fun c ->
  Ok [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"; ".o"; ".cma"; ".cmxa"; ".a"]) "src/Immutable" ~dst:"immutable";

  ]
