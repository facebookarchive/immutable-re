(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Topkg

let () =

  let cmd c os files =
    let ocamlbuild = Conf.tool "rebuild" os in
    OS.Cmd.run @@ Cmd.(ocamlbuild % "-use-ocamlfind"
                                  %% (v "-I" % "immutable/src/core")
                                  %% (v "-I" % "immutable/src/extra")
                                  %% (v "-I" % "immutable/src/indexed")
                                  %% (v "-I" % "immutable/src/utils")
                                  %% (v "-I" % "immutable/src/map")
                                  %% (v "-I" % "immutable/src/set")
                                  %% of_list files)
  in
  let build = Pkg.build ~cmd () in
  Pkg.describe "immutable" ~build ~change_logs:[] ~licenses:[] ~readmes:[] @@ fun c ->
  Ok [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"; ".o"; ".cma"; ".cmxa"; ".a"]) "immutable/src/Immutable" ~dst:"immutable";

  ]
