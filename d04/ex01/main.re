let main = () => {
  let rec allToInt = l =>
    switch (l) {
    | [h, n, ...t] =>
      print_int(Value.toInt(h));
      print_char('\n');
      allToInt([n, ...t]);
    | [h, ...t] =>
      print_int(Value.toInt(h));
      print_char('\n');
    | [] => print_int(0)
    };

  allToInt(Value.all);
  print_char('\n');

  let rec allToString = l =>
    switch (l) {
    | [] => print_char('\n')
    | [h, ...t] =>
      print_endline(Value.toString(h));
      allToString(t);
    };

  allToString(Value.all);

  let rec allToStringVerbose = l =>
    switch (l) {
    | [] => print_char('\n')
    | [h, ...t] =>
      print_endline(Value.toStringVerbose(h));
      allToStringVerbose(t);
    };

  allToStringVerbose(Value.all);

  let rec allNext = l =>
    switch (l) {
    | [] => print_char('\n')
    | [h, ...t] when h == Value.As =>
      print_endline("As has no next card");
      allNext(t);
    | [h, ...t] =>
      print_string(Value.toString(h));
      print_string(" : ");
      print_endline(Value.toString(Value.next(h)));
      allNext(t);
    };

  allNext(Value.all);

  let rec allPrevious = l =>
    switch (l) {
    | [] => print_char('\n')
    | [h, ...t] when h == Value.T2 =>
      print_endline("T2 has no previous card");
      allPrevious(t);
    | [h, ...t] =>
      print_string(Value.toString(h));
      print_string(" : ");
      print_endline(Value.toString(Value.previous(h)));
      allPrevious(t);
    };

  allPrevious(Value.all);
};

let () = main();
