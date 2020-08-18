let crossover = (lst1, lst2) =>
  if (lst1 == [] || lst2 == []) {
    [];
  } else {
    let rec crossover_1 = (res, liste_1) =>
      switch (liste_1) {
      | [] => res
      | [head1, ...tail1] =>
        let rec crossover_2 = (res_appended, liste_2) =>
          switch (liste_2) {
          | [] => crossover_1(res_appended, tail1)
          | [head2, ...tail2] when head2 != head1 =>
            crossover_2(res_appended, tail2)
          | [head2, ...tail2] => crossover_2(res_appended @ [head2], tail2)
          };

        crossover_2(res, lst2);
      };

    crossover_1([], lst1);
  };

let rec print_result_int =
  fun
  | [] => ()
  | [e, ...l] => {
      print_int(e);
      print_string(", ");
      print_result_int(l);
    };

let rec print_result_str =
  fun
  | [] => ()
  | [e, ...l] => {
      print_string(e);
      print_string(", ");
      print_result_str(l);
    };

let main = () => {
  let a = [12, 3, 0, 7, (-3), 9, (-1)];
  let b = [4, (-2), 9, 13, 12, (-3), 11];
  let c = [];
  let x = ["toto", "tata", "titi", "toutou", "gotgot", "kikou", ""];
  let y = ["tamtam", "tutu", "gotgot", "toto", "gregou", "tata", "liloulol"];
  print_result_int(crossover(a, b));
  print_char('\n');
  print_result_int(crossover(a, c));
  print_char('\n');
  print_result_str(crossover(x, y));
  print_char('\n');
};

let () = main();
