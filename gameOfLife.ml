#use "list_tools.ml";;

let new_cell = 1 ;;
let empty = 0 ;;
let is_alive cell = cell <> empty ;;
let cell_size = 10;;

let rules0 cell near = match (cell, near) with
    (0,3) -> new_cell
  | (1,2) | (1,3) -> new_cell
  | _ -> empty;;

(* let count_neighbours (x,y) board size =
  let count = function
      (x,y) when (x < 0 || y < 0) -> 0
    | (x,y) when (x >= size || y >= size) -> 0
    | (x,y) -> get_cell (x,y) board
  in count (x,y+1) + count (x,y-1) + count (x-1,y) + count (x-1,y-1)
  + count (x-1,y+1) + count (x+1,y) + count (x+1,y-1) + count (x+1,y+1) ;; *)


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



let seed_life board size nb_cell =
  let rec seed board n = match n with
      0 -> board
    | n -> seed (put_cell new_cell (Random.int size, Random.int size) board) (n-1)
  in seed board nb_cell ;;

let new_board size nb_cell =
  seed_life (init_board (size, size) 0) size nb_cell;;

(* let next_generation board size =
  let rec next_list x y = function
      [] -> []
    | e::l -> rules0 e (count_neighbours (x,y) board size)::next_list x (y+1) l

  in let rec next_board x y = function
      _ when x = size -> []
    | e::l -> next_list x y e::next_board (x+1) y board
    | _ -> invalid_arg "next_generation : erreur d'argument"
  in next_board 0 0 board;; *)

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
  | _ -> draw_board board cell_size;
         game (next_generation board size) size (n-1);;

let new_game size nb_cell n =
  open_window (size * cell_size + 40);
  game (new_board size nb_cell) size n;;

(* new_game 50 200 500;; *)
