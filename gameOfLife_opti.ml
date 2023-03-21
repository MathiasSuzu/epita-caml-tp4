(*
    #####################
    #     graphics      #
    #####################
 *)
# use "topfind" ;;
# require " graphics" ;;

open Graphics;;

let open_window size = open_graph (" " ^ string_of_int size ^ " x " ^ string_of_int (size +20));;
(*
    #####################
    #     préruquis     #
    #####################
 *)
let cell_size = 10;;

let cell_color = function
| 0 -> white
| _ -> black ;;

let fill_square (x,y) size =
  let grey = rgb 127 127 127 in
  fill_rect x y size size;
  set_color grey;
  moveto x y;
  draw_rect x y size size;;

let draw_cell (x,y) size color =
  set_color (cell_color color);
  fill_square (x,y) size;;

let draw_board board cellsize =
  let rec board_rec board x y = match board with
      [] -> ()

    | []::l     -> board_rec l (x+cellsize) 10

    | (e::l)::d -> draw_cell (x,y) cellsize e;
                   board_rec (l::d) x (y+cellsize);
  in board_rec board 10 10;;

let rules0 cell near = match (cell, near) with
    (0,3) -> 1
  | (1,2) | (1,3) -> 1
  | _ -> 0;;

  let init_list n x =
    if n < 0 then invalid_arg("init_list : n must be a natural") else
     let rec init n =
       if n = 0 then [] else
         x::init (n-1)
     in init n;;
   
   let init_board (l,c) x =
     let c_list = init_list c x in
     init_list l c_list ;;
   
   let nth n list =  (* nth qui donne la valeur du nieme' élément d’une liste list. *)
     if n < 0 then invalid_arg("nth: index must be a natural") else
       let rec nt n list = match (n,list) with
         (_,[])    -> failwith("nth: list is too short")
       | (0, e::_) -> e
       | (_, _::l) -> nt (n-1) l
       in nt n list;;
   
   let put_list v i list =
       let rec put i list = match (i,list) with
         (_,[])    -> []
       | (0, _::l) -> v::l
       | (_, e::l) -> e::put (i-1) l
       in put i list;;
   
   let put_cell v (x, y) board =
     let y_list = nth x board in
     let nouveau = put_list v y y_list in
     put_list nouveau x board ;;
   
   let seed_life board size nb_cell =
     let rec seed board n = match n with
         0 -> board
       | n -> seed (put_cell 1 (Random.int size, Random.int size) board) (n-1)
     in seed board nb_cell ;;
   
   let new_board size nb_cell =
     seed_life (init_board (size, size) 0) size nb_cell;;

(*
    #####################
    #   Optimisations   #
    #####################
 question 1 et 2
 *)

let count_neighbours (x,y) board size =
  if x >= size || y >= size || x < 0 || y < 0 then invalid_arg "count_neighbours : postion not good" else
  let rec count a b list = match list with
       | _::l when b < y-1 -> count a (b+1) l
       | l when b > y+1 -> 0
       | e::l when (a=x && b=y) -> count a (b+1) l
       | e::l  -> e + count a (b+1) l
       | [] -> 0
  in let rec defile a board = match board with
       | _::l when a < x-1 -> defile (a+1) l
       | l when a > x+1 -> 0
       | e::l -> count a 0 e + defile (a+1) l
       | [] -> 0
     in defile 0 board;;

let draw_board_opti board cellsize next_gen =
 let rec draw board x y next_gen =
   match (board, next_gen) with
   ([],_) | (_,[]) -> ()
   | ([]::l,[]::g) -> draw l (x+cellsize) 10 g
   | ((e::l)::d, (i::g)::w) when e = i -> draw (l::d) x (y+cellsize) (g::w)
   | ((e::l)::d, (i::g)::w) -> draw_cell (x,y) cellsize i;
                               draw (l::d) x (y+cellsize) (g::w)
   | _ -> failwith "bord and next_gen not same length"
 in draw board 10 10 next_gen;;


let next_generation board size =
  let rec next_list x y list = match list with
                | [] -> []
                | e::lst -> rules0 e (count_neighbours (x,y) board size)::next_list x (y+1) lst
                    in
  let rec next x y board = match board with
    | [] -> []
    | e::l -> next_list x y e::next (x+1) 0 l
  in next 0 0 board;;


let rec game board size n = match n with
    0 -> ()
  | _ -> let next = next_generation board size in
         draw_board_opti board cell_size next;
         game next size (n-1);;

let new_game size nb_cell n =
  let board = new_board size nb_cell in
  open_window (size * cell_size + 40);
  draw_board board cell_size;
  game board size n;;

(*
    ############################
    # entrer/sorti et patterns #
    ############################
*)

let load name =
    let ic = open_in name in
    let try_read () =
    try Some (input_line ic ) with End_of_file -> None in
      let rec loop () = match try_read () with
        Some s -> s ::( loop () )
        | None -> close_in ic ; []
    in loop () ;;

let load_board filename =
  let str = (load filename)  in
  let rec decomp n e = match e.[n] with
    | '[' | ']' | ';' |' ' -> decomp (n+1) e
    | '1' -> 1::decomp (n+1) e
    | '0' -> 0::decomp (n+1) e
    | _ -> []
  in let rec  test str = match str with
    [] -> []
    | e::l -> decomp 0 (e ^ "f"):: test l
  in test str;;

let save_board filename board =
  let oc = open_out filename in
  let rec save = function
      []::[] ->  Printf.fprintf oc "%s" "]]";close_out oc

    | []::l     -> Printf.fprintf oc "%s" "];\n[" ; save l
    | (e::[])::d -> Printf.fprintf oc "%s" ((string_of_int e)) ; save ([]::d)
    | (e::l)::d -> Printf.fprintf oc "%s " ((string_of_int e) ^ ";") ; save (l::d)
    | [] -> close_out oc
  in Printf . fprintf oc "%s" "[[";save board;;

let init_pattern pattern size =
  let raw_board = init_board (size,size) 0 in
  let rec init board = function
      [] -> board
    | (x,y)::_ when x >= size || y >= size || x < 0 || y < 0 -> invalid_arg "position out of bound"
    | e::l ->  init (put_cell 1 e board) l
   in init raw_board pattern;;

let new_game_pattern board size nb =
  open_window (size * cell_size + 40);
  game board size nb;;

let rec remaining = function
    [] -> false
    | (e::_)::_ when e > 0 -> true
    | []::d -> remaining d
    | (_::l)::d -> remaining (l::d);;

let rec game_survival board size= match remaining board with
    false -> draw_board board cell_size
  | _ -> let next = next_generation board size in
         draw_board_opti board cell_size next;
         game_survival next size

let new_game_survival size nb_cells =
  let board = new_board size nb_cells in
  open_window (size * cell_size + 40);
  draw_board board cell_size;
  game_survival board size;;

let new_game_pattern_survival board size =
  open_window (size * cell_size + 40);
  draw_board board cell_size;
  game_survival board size;;