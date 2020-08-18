let print_example = (example: (array(float), string)): unit => {
  print_string("([|");
  let rec print_array = (l: list(float)) =>
    switch (l) {
    | [h, n, ...t] =>
      Printf.printf("%f; ", h);
      print_array([n, ...t]);
    | [h, ...t] => Printf.printf("%f|]", h)
    | _ => ()
    };

  print_array(Array.to_list(fst(example)));
  Printf.printf(", %s)\n", snd(example));
};

let rec print_serie = (serie: list((array(float), string))) =>
  switch (serie) {
  | [] => print_char('\n')
  | [h, ...t] =>
    print_example(h);
    print_serie(t);
  };

let examples_of_file = (filename: string): list((array(float), string)) => {
  let file = open_in(filename);
  let result = ref([]);
  try(
    {
      while (true) {
        let example = input_line(file);
        let listOfStrings = String.split_on_char(',', example);
        let listRev = List.rev(listOfStrings);
        let arrayFloats =
          Array.of_list(
            List.map(float_of_string, List.rev(List.tl(listRev))),
          );
        let pair = (arrayFloats, List.hd(listRev));
        result := [pair, ...result^];
      };
      result^;
    }
  ) {
  | Sys_error(err) =>
    close_in(file);
    Printf.printf("Something wrong happened : %s\n", err);
    [];
  | End_of_file =>
    close_in(file);
    List.rev(result^);
  };
};

let main = (argc: int, argv: array(string)) =>
  switch (argc) {
  | x when x != 2 => Printf.printf("Usage: ./a.out filename")
  | _ => print_serie(examples_of_file(argv[1]))
  };

let () = {
  let argv = Sys.argv;
  main(Array.length(argv), argv);
};
