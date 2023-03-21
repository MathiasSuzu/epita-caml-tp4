(* 

   ################
   #  les listes  #
   ################ 

*)


let rec length = function
  []  -> 0
  | _::l -> length l + 1;;

let nth n list =  (* nth qui donne la valeur du nieme' élément d’une liste list. *)
  if n < 0 then invalid_arg("nth: index must be a natural") else
    let rec nt n list = match (n,list) with
      (_,[])    -> failwith("nth: list is too short")
    | (0, e::_) -> e
    | (_, _::l) -> nt (n-1) l
  in nt n list;;

(* let rec prefix list1 list2 = match (list1,list2) with
  (_, []) | ([], _) -> true
  |(e::l1, f::l2) when e = f -> prefix l1 l2
  | _ -> false ;; *)

let init_list n x = 
  if n < 0 then invalid_arg("init_list : n must be a natural") else
  let rec init n = 
    if n = 0 then [] else 
      x::init (n-1)
  in init n;;

(* let rec append list1 list2 = match list1 with
    []   -> list2
  | e::l -> e::append l list2;; *)

 let put_list v i list =
    let rec put i list = match (i,list) with
      (_,[])    -> []
    | (0, _::l) -> v::l
    | (_, e::l) -> e::put (i-1) l
  in put i list;;

(* 

   ################
   # les matrices #
   ################ 

*)

let init_board (l,c) x = 
  let c_list = init_list c x in 
  init_list l c_list ;;
  
let get_cell (x, y) board = 
  let x_list = nth x board in
  nth y x_list ;;

let put_cell v (x, y) board =
  let y_list = nth x board in
  let nouveau = put_list v y y_list in
  put_list nouveau x board ;;


(*

   ################
   # jeu de la vie #
   ################

*)

let new_cell = 1 ;; (*alivecell*)
let empty = 0 ;;
let is_alive cell = cell <> empty ;;

# use "topfind" ;; 
# require " graphics" ;;

open Graphics;;

let open_window size = open_graph ( " " ^ string_of_int size ^ " x " ^
                                      string_of_int (size +20)) ;;
(* open_window 10;; *)

let open_window size = open_graph (" " ^ string_of_int size ^ "x" ^
                                   string_of_int (size+20)) ;;

let grey = rgb 127 127 127 ;;

let fill_square (x,y) size =
  fill_rect x y size size;
  set_color grey;
  moveto x y;
  draw_rect x y size size;;

let cell_color = function
| 0 -> white
| _ -> black ;;

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