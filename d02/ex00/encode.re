let encode = lst => {
  let rec encode_aux = (current_lst, new_lst, n) =>
    switch (current_lst) {
    | [] => new_lst
    | [head, next, ...tail] when head == next =>
      encode_aux([next, ...tail], new_lst, n + 1)
    | [head, next, ...tail] =>
      encode_aux([next, ...tail], new_lst @ [(n + 1, head)], 0)
    | [head, ...t] => encode_aux([], new_lst @ [(n + 1, head)], 0)
    };

  encode_aux(lst, [], 0);
};

let rec print_result_int =
  fun
  | [] => ()
  | [e, ...l] => {
      print_char('(');
      print_int(fst(e));
      print_string(", ");
      print_int(snd(e));
      print_string("), ");
      print_result_int(l);
    };

let rec print_result_char =
  fun
  | [] => ()
  | [e, ...l] => {
      print_char('(');
      print_int(fst(e));
      print_string(", ");
      print_char(snd(e));
      print_string("), ");
      print_result_char(l);
    };

let main = () => {
  let l1 = [3, 4, 4, 4, 5, 7, 8, 8, 8, 8];
  let l2 = ['a', 'a', 'b', 'c', 'c', '!'];
  let l3 = [];
  print_result_int(encode(l1));
  print_char('\n');
  print_result_char(encode(l2));
  print_char('\n');
  print_result_int(encode(l3));
  print_char('\n');
};

let () = main();
