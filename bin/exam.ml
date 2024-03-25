(*SAMOURA Amadou Aternant en Ing DeVops chez Societé Generale*)



[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-69"]
let _ = Random.self_init ()
type followers_assoc = (string * string list) list

(* ..........................PARTIE A ............................*)
(*  let is_alphanum (c : char) : bool = failwith "TODO"   *)

(* Question 1 *)

let is_alphanum c =
match c with
| 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
| _ -> false;;


(* let words (str : string) : string list = failwith "TODO"    *)

(* Question 2 *)
let words sentence =
let clean_char c =
  if is_alphanum c then c else ' '
in
let cleaned_sentence = String.map clean_char sentence in
String.split_on_char ' ' cleaned_sentence
|> List.filter (fun word -> word <> "");;


(*  let add_follower_assoc (word : string) (follower : string)
    (followers : followers_assoc) : followers_assoc =failwith "TODO"   *)

(* Question 3 *)
let add_follower_assoc word follower table =
let rec update_assoc word follower table_acc =
  match table_acc with
  | [] -> [(word, [follower])]
  | (w, followers) :: tl ->
    if w = word then (w, follower :: followers) :: tl
    else (w, followers) :: (update_assoc word follower tl)
in
update_assoc word follower table;;

(* let build_table_assoc (words_list : string list) : followers_assoc =
  failwith "TODO"  *)

  (* Question 4 *)
let build_assoc words_list =
let rec aux words acc =
  match words with
  | [] -> acc
  | [last_word] -> add_follower_assoc "STOP" last_word acc
  | word1 :: word2 :: tl ->
    let acc' = add_follower_assoc word1 word2 acc in
    aux (word2 :: tl) acc'
in
aux ("START" :: words_list) [];;

(*  let get_follower_assoc (table : followers_assoc) (word : string) : string =
  failwith "TODO"   *)

(* Question 5 *)

let get_follower_assoc table word =
let followers = List.assoc_opt word table in
match followers with
| Some follower_list ->
  let followers_count = List.length follower_list in
  let random_index = Random.int followers_count in
  List.nth follower_list random_index
| None -> failwith "Pas de mot trouver dans la table ";;

(*let generate_text_assoc (table : followers_assoc) : string list =
  failwith "TODO"*)

(* Question 6 *)
let rec generate_text_assoc table =
  let rec generate_aux word acc =
    if word = "STOP" then
      List.rev acc
    else
      let next_word = get_follower_assoc table word in
      generate_aux next_word (next_word :: acc)
  in
  generate_aux "START" [];;


  (* Exemples *)
  (*
  let text_table = build_assoc (words "I am a man and my dog is a good dog")
  let generated_text = generate_text_assoc text_table
  print_quote generated_text
  *)


(* ........................PARTIE B .......................................*)

(* module StrMap =  *)

(* Question 7 *)
type distribution = { occurrences : int; followers : (string * int) list }
type followers_map = distribution StrMap.t;;

module StrMap = Map.Make(String);; 

(* Question 8 *)
type distribution = { occurrences : int; followers : (string * int) list }

let compute_distribution (words : string list) : distribution = failwith "TODO";;

let compute_distribution words_list =
  let sorted_words = List.sort compare words_list in
  let rec count_occurrences acc curr = function
    | [] -> { occurrences = List.length curr; followers = acc }
    | [w] -> count_occurrences ((w, 1) :: acc) (w :: curr) []
    | w1 :: w2 :: rest ->
        if w1 = w2 then count_occurrences acc (w1 :: curr) (w2 :: rest)
        else count_occurrences ((w1, List.length curr) :: acc) [w2] rest
  in
  count_occurrences [] [] sorted_words;;


(* Question 9 *)

(*let build_table_map (words_list : string list) : followers_map = failwith "TODO";;*)

type followers_map = distribution StrMap.t

let add_follower_map word follower map =
  match StrMap.find_opt word map with
  | None -> StrMap.add word { occurrences = 1; followers = [(follower, 1)] } map
  | Some dist ->
      let updated_followers =
        match List.assoc_opt follower dist.followers with
        | None -> (follower, 1) :: dist.followers
        | Some count -> (follower, count + 1) :: List.remove_assoc follower dist.followers
      in
      StrMap.add word { occurrences = dist.occurrences + 1; followers = updated_followers } map

let build_table_map words_list =
  let rec build_map map = function
    | [] -> map
    | [w] -> add_follower_map w "STOP" map
    | w1 :: w2 :: rest -> build_map (add_follower_map w1 w2 map) (w2 :: rest)
  in
  build_map StrMap.empty words_list;;


(* Question 10 *)

(*let get_follower_map (table : followers_map) (word : string) : string =
  failwith "TODO"*)

let get_follower_map table word =
  match StrMap.find_opt word table with
  | None -> failwith "Pas de mot trouver dans la table "
  | Some dist ->
      let total_followers = List.fold_left (fun acc (_, count) -> acc + count) 0 dist.followers in
      let random_index = Random.int total_followers in
      let rec find_follower index = function
        | [] -> failwith "No followers found" (* Shouldn't happen *)
        | (follower, count) :: t ->
            if index < count then follower
            else find_follower (index - count) t
      in
      find_follower random_index dist.followers;;


(* Question 11 *)

(*let generate_text_map (table : followers_map) : string list = failwith "TODO"*)

let generate_text_map (table : followers_map) : string list = failwith "TODO";;

let rec generate_text_map table =
  let rec generate_aux word acc =
    if word = "STOP" then
      List.rev acc
    else
      let next_word = get_follower_map table word in
      generate_aux next_word (next_word :: acc)
  in
  generate_aux "START" [];;



(* ............................Partie C................................ *)


type kind = Valid | Terminate | Single | Separator

let kind_of_char (c : char) : kind = failwith "TODO";;



(* module SLMap = *)

let shift (words : string list) (next : string) : string list = failwith "TODO";;



(* Question 13 *)

let sentences (text : string) : string list list = failwith "TODO";;

let sentence text =
  let rec construct_sentence char kind acc_sent acc_word = function
    | [] -> (match kind with
             | Separator -> if acc_word <> "" then List.rev ((List.rev (acc_word :: acc_sent)) :: [])
                            else List.rev acc_sent
             | _ -> if acc_word <> "" then List.rev ((List.rev acc_word) :: acc_sent) else List.rev acc_sent)
    | c :: t ->
        let new_kind = kind_of_char c in
        match kind, new_kind with
        | Valid, Separator -> construct_sentence c new_kind (acc_word :: acc_sent) "" t
        | Separator, Separator -> construct_sentence c new_kind acc_sent "" t
        | _ -> construct_sentence c new_kind acc_sent (acc_word ^ String.make 1 c) t
  in
  construct_sentence ' ' Separator [] "" (List.init (String.length text) (String.get text));;




(* Question 14 *)
module SLMap = Map.Make(struct type t = string list let compare = compare end);;

(* Question 15 *)
let start (n : int) : string list = failwith "TODO";;

let start n =
  let rec start_aux acc i =
    if i = 0 then acc
    else start_aux ("START" :: acc) (i - 1)
  in
  start_aux [] n;;


(* Question 16 *)
type followers_multi = { prefix_length : int; table : distribution SLMap.t }

let build_table_multi (sentences : string list list) (prefix_length : int) :
    followers_multi =
  failwith "TODO";;

  let shift lst new_elem =
    match lst with
    | [] -> failwith "Empty list"
    | _ :: t -> List.append t [new_elem];;


  (* Question 17 *)


  let get_follower_multi ({ table; _ } : followers_multi) (prefix : string list) =
    failwith "TODO";;

  type followers_multi = { prefix_length : int; table : distribution SLMap.t }

  let build_table_multi_words word_list ptable prefix_length =
    let rec build_table map words =
      match words with
      | [] -> map
      | _ :: [] -> map
      | prefix :: next :: rest ->
          let new_map =
            match SLMap.find_opt prefix map with
          | None -> SLMap.add prefix { occurrences = 1; followers = [(next, 1)]}map
          | Some dist ->
              let updated_followers =
              match List.assoc_opt next dist.followers with
              | None -> (next, 1) :: dist.followers
             | Some count -> (next, count + 1) :: List.remove_assoc next dist.followers
                in SLMap.add prefix { occurrences = dist.occurrences + 1; followers = updated_followers } map
          in build_table new_map (next :: rest)
    in build_table ptable word_list
  let build_table_multi sentences prefix_length =let ptable =
      List.fold_left (fun acc sentence -> build_table_multi_words sentence acc prefix_length) SLMap.empty sentences
    in { prefix_length = prefix_length; table = ptable }


  (* Question 18 *)
  let generate_text_multi (table : followers_multi) : string list =
  failwith "TODO";;

  let rec generate_text_multi ptable =
    let rec generate_aux prefix acc =
      if SLMap.mem prefix ptable.table then
        let dist = SLMap.find prefix ptable.table in
        let total_followers = List.fold_left (fun acc (_, count) -> acc + count) 0 dist.followers in
        let random_index = Random.int total_followers in
        let rec find_follower index = function
          | [] -> failwith "No followers found" (* Shouldn't happen *)
          | (follower, count) :: t ->
              if index < count then follower
              else find_follower (index - count) t
        in
        let next_word = find_follower random_index dist.followers in
        if next_word = "STOP" then List.rev acc
        else generate_aux (shift prefix next_word) (next_word :: acc)
      else List.rev acc
    in
    let start_prefix;;




let bindings_map (table : followers_map) = StrMap.bindings table
let bindings_multi (table : followers_multi) = SLMap.bindings table
