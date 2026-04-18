let h : (int, int * int) Hashtbl.t = Hashtbl.create 1
let f : (int, int * int option) Hashtbl.t = Hashtbl.create 1

let rec print_product = function
  | (p, None) -> 
    print_endline (Int.to_string p)
  | (p, Some y) ->
    print_string (Int.to_string p);
    print_string " * ";
    print_product (Hashtbl.find f y)
  ;;

let rec loop k =
  if k > 100 then () else (
    let c = ref (k, None) in
    begin
      match Hashtbl.find_all h k with
      | [] -> 
        print_endline (Int.to_string k ^ " is prime");
        Hashtbl.add h (k + k) (k, 2)
      | pys ->
        Hashtbl.remove h k;
        List.iter (fun (p, y) ->
          c := (p, Some y);
          Hashtbl.add h (k + p) (p, y + 1)
        ) pys
    end;
    begin
      match !c with
      | (p, None) -> ()
      | product ->
        print_string (Int.to_string k);
        print_string " = ";
        print_product product
    end;
    Hashtbl.add f k !c;
    loop (k + 1))
;;

let () = loop 2