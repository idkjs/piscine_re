let selectJoke = (a: array(string), len: int) => {
  Random.self_init();
  let idx = Random.int(len);
  a[idx];
};

let main = (argc: int, argv: array(string)) =>
  switch (argc) {
  | x when x != 2 => print_endline("Usage: ./a.out name_of_file")
  | _ =>
    let filename = argv[1];
    let channel = open_in(filename);
    let arrayLen = ref(0);
    try(
      while (true)
        {
          ignore(input_line(channel));
          incr(arrayLen);
        }
        /* let a = Array.make 1 "" in let result = Array.append result a in
           a.(0) <- input_line channel ; ignore (result) */
    ) {
    | End_of_file =>
      {
        seek_in(channel, 0);
        let a = Array.make(arrayLen^, "");
        try(
          {
            for (i in 0 to arrayLen^ - 1) {
              let str = input_line(channel);
              a[i] = str;
            };
            print_endline(selectJoke(a, arrayLen^));
          }
        ) {
        | Sys_error(err) =>
          Printf.printf("Error: Something went wrong : %s\n", err)
        | End_of_file => Printf.printf("Error: reached end of file\n")
        };
      };
      close_in(channel);
    };
  };

let () = {
  let argv = Sys.argv;
  main(Array.length(argv), argv);
};
