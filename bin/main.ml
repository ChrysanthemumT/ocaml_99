[@@@warning "-32-27-37"]

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

let rec length list = match list with [] -> 0 | x :: v -> 1 + length v

let rev list =
  let rec help acc listt =
    match listt with [] -> acc | x :: v -> help (x :: acc) v
  in
  help [] list

let is_palindrome list = list = rev list

type 'a node = One of 'a | Many of 'a node list

let flatten list =
  let rec help acc = function
    | [] -> acc
    | One x :: v -> help (x :: acc) v
    | Many l :: v -> help (help acc l) v
  in
  rev @@ help [] list

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

let pack list =
  let rec aux acc1 acc2 listt =
    match listt with
    | [] -> acc2 :: acc1
    | a :: (b :: _ as t) ->
        if a = b then aux acc1 (a :: acc2) t else aux ((a :: acc2) :: acc1) [] t
    | smaller -> smaller :: acc1
  in
  rev @@ aux [] [] list

let encode list =
  let packed = pack list in
  let rec aux acc = function
    | [] -> acc
    | [] :: t -> aux acc t
    | (x :: _ as a) :: v -> aux ((length a, x) :: acc) v
  in
  rev @@ aux [] packed

type 'a rle = One of 'a | Many of int * 'a

let n_encoded list =
  let packed = pack list in
  let rec aux acc = function
    | [] -> acc
    | [] :: t -> aux acc t
    | (x :: _ as a) :: v ->
        if length a = 1 then aux (One x :: acc) v
        else aux (Many (length a, x) :: acc) v
  in
  rev @@ aux [] packed

let decode l =
  let rec gen count x acc =
    if count = 0 then acc else gen (count - 1) x (x :: acc)
  in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (gen 1 x acc) t
    | Many (a, b) :: t -> aux (gen a b acc) t
  in
  rev @@ aux [] l

let rec duplicate = function [] -> [] | x :: t -> x :: x :: duplicate t

let replicate l count =
  let rec aux acc c = function
    | [] -> acc
    | x :: t as b -> if c = 0 then aux acc count t else aux (x :: acc) (c - 1) b
  in
  rev @@ aux [] count l

let drop l c =
  let rec aux acc count = function
    | [] -> acc
    | x :: t -> if count = 1 then aux acc c t else aux (x :: acc) (count - 1) t
  in
  rev @@ aux [] c l

let split l c =
  let rec aux acc f count = function
    | [] -> [] :: f :: acc
    | [ x ] -> if count = 0 then [ x ] :: acc else (rev @@ (x :: f)) :: acc
    | x :: t as b ->
        if count = 0 then b :: (rev @@ f) :: acc
        else aux acc (x :: f) (count - 1) t
  in
  rev @@ aux [] [] c l

let slice l i k =
  let rec aux acc ind = function
    | [] -> acc
    | x :: t ->
        if ind >= i && ind <= k then aux (x :: acc) (ind + 1) t
        else aux acc (ind + 1) t
  in
  rev @@ aux [] 0 l

let rec rotate list count =
  match list with
  | [] -> []
  | x :: t as a -> if count = 0 then a else rotate (t @ [ x ]) (count - 1)

let rec remove_at id = function
  | [] -> []
  | x :: t -> if id = 0 then t else x :: remove_at (id - 1) t

let () =
  let x = 1 in
  Printf.printf "%d" x
