let rec ft_print_string_of_list =
  fun
  | [] => print_char('\n')
  | [head, ...tail] => {
      print_char(head);
      ft_print_string_of_list(tail);
    };

let sequence = n =>
  if (n < 1) {
    ft_print_string_of_list([]);
  } else {
    let rec new_list = (lst_in, lst_out, count) =>
      switch (lst_in) {
      | [] => lst_out
      | [head, next, ...tail] when head == next =>
        new_list([next, ...tail], lst_out, count + 1)
      | [head, ...tail] =>
        new_list(
          tail,
          lst_out @ [char_of_int(int_of_char('0') + count + 1), head],
          0,
        )
      };

    let rec sequence_aux = (x, res) =>
      switch (x) {
      | y when y == n => ft_print_string_of_list(res)
      | _ => sequence_aux(x + 1, new_list(res, [], 0))
      };

    sequence_aux(1, ['1']);
  };

let () = {
  sequence(0);
  sequence(1);
  sequence(2);
  sequence(3);
  sequence(4);
  sequence(5);
  sequence(6);
  sequence(7);
};
