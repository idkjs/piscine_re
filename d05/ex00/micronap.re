let my_sleep = () => Unix.sleep(1);

let main = (argc: int, argv: array(string)) =>
  switch (argc) {
  | x when x == 2 =>
    let timer =
      try(int_of_string(argv[1])) {
      | Failure(err) =>
        raise(Invalid_argument("Error : please specify an interger."))
      };
    for (i in timer downto 0) {
      my_sleep();
    };
  | x when x < 2 => raise(Invalid_argument("Error : Missing argument"))
  | _ => raise(Invalid_argument("Error : Too many arguments"))
  };

let () = {
  let argv = Sys.argv;
  try(main(Array.length(argv), argv)) {
  | Invalid_argument(err) => print_endline(err)
  | _ => ()
  };
};

/* ocamlopt unix.cmxa micronap.ml */
