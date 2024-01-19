open Char_list

type interval =
  { first : int
  ; last : int
  }

(** Smart constructor for intervals. Raises [Invalid_argument] if [first] is
    greater than [last]. *)
let mk (first, last) =
  if first <= last
  then { first; last }
  else raise (Invalid_argument "mk: first must be less than or equal to last")
;;

(** Returns true if the number [i] is inside any of the intervals in the given
    [table]. The input [table] must be sorted. *)
let binary_search (table : interval list) (i : int) : bool =
  let min, max = 0, List.length table - 1 in
  if i < (List.hd table).first || i > (List.nth table max).last
  then false
  else (
    let rec inner_search min max =
      if max < min
      then false
      else (
        let mid = (min + max) / 2 in
        if i > (List.nth table mid).last
        then inner_search (mid + 1) max
        else if i < (List.nth table mid).first
        then inner_search min (mid - 1)
        else true)
    in
    inner_search min max)
;;

let zero_width_chars = List.map mk Char_list.zero_width_chars
let wide_chars = List.map mk Char_list.wide_chars

let wcwidth (c : Uchar.t) =
  match Uchar.to_int c with
  | 0 -> 0
  | i when i < 32 || (i >= 0x7f && i < 0x80) -> -1
  | i when List.mem i zero_width_others -> 0
  | i when binary_search zero_width_chars i -> 0
  | i when binary_search wide_chars i -> 2
  | _ -> 1
;;

let to_utf8 (s : string) : Uchar.t list =
  (* String.length gives the number of bytes *)
  let len = String.length s in
  let rec aux n accum =
    (* If we have read the whole string, return the accumulated list *)
    if n >= len
    then accum
    else (
      (* Split off the first UTF-8 character *)
      let decode = String.get_utf_8_uchar s n in
      let uc = Uchar.utf_decode_uchar decode in
      let sz = Uchar.utf_decode_length decode in
      uc :: aux (n + sz) accum)
  in
  aux 0 []
;;

let wcswidth (s : string) =
  let uchars = to_utf8 s in
  List.fold_right
    (fun uc acc ->
      let w = wcwidth uc in
      if w = -1 || acc = -1 then -1 else acc + w)
    uchars
    0
;;
