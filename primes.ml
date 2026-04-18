let h : (int, int * int) Hashtbl.t = Hashtbl.create 1

let rec loop k =
  if k > 100 then () else (
  begin
    match Hashtbl.find_all h k with
    | [] -> 
      print_string (Int.to_string k);
      print_char ' ';
      Hashtbl.add h (k + k) (k, 2)
    | pys ->
      Hashtbl.remove h k;
      List.iter (fun (p, y) ->
        Hashtbl.add h (k + p) (p, y + 1)
      ) pys
  end;
  loop (k + 1)
  );;

let () = loop 2