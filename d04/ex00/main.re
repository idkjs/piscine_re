let main = () => {
  let rec printAll = l =>
    switch (l) {
    | [] => print_char('\n')
    | [h, ...t] =>
      {
        print_endline(Color.toString(h));
        print_endline(Color.toStringVerbose(h));
      };
      printAll(t);
    };

  printAll(Color.all);
};

let () = main();
