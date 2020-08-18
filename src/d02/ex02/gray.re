let gray = n =>
  if (n < 1) {
    print_endline("Error");
  } else {
    let limit = int_of_float(2. ** float_of_int(n));

    let rec print_output = (x, loop_nbr) => {
      let q = x / 2;
      let r = x mod 2;
      switch (q) {
      | 0 when loop_nbr == n || n == 0 => print_int(r)
      | 0 =>
        print_int(0);
        print_output(x, loop_nbr + 1);
      | _ =>
        print_output(q, loop_nbr + 1);
        print_int(r);
      };
    };

    let rec gray_aux = decimal =>
      switch (decimal) {
      | y when y == limit => print_char('\n')
      | _ =>
        print_output(decimal lxor decimal lsr 1, 1);
        print_char(' ');
        gray_aux(decimal + 1);
      };

    gray_aux(0);
  };

let main = () => {
  gray(0);
  gray(1);
  gray(2);
  gray(3);
  gray(4);
  gray(5);
};

let () = main();
