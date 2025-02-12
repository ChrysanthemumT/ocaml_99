[@@@warning "-32-27"]

let rec last list =
  match list with [] -> None | [ x ] -> Some x | _ :: v -> last v

let rec last_two list =
  match list with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: v -> last_two v

let rec at ind list =
  match list with
  | [] -> None
  | x :: v -> if ind = 0 then Some x else at (ind - 1) v

let () =
  match last_two [ 1; 2; 3; 4 ] with
  | Some (x, y) -> Printf.printf "%d %d\n" x y
  | None -> Printf.printf "Empty list\n"
