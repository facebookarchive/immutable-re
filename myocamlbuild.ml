open Solvuu_build.Std

let project_name = "immutable"
let version = "0.0.1"

let mylib = Project.lib project_name
  ~dir:"immutable"
  ~style:(`Pack "immutable")

let () = Project.basic1 ~project_name ~version [mylib]
