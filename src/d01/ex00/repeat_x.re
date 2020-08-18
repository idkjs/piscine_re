let repeat_x = n =>
  if (n < 0) {
    "Error";
  } else {
    let rec loop = (x, acc) =>
      if (x === 0) {
        acc;
      } else {
        loop(x - 1, acc ++ "x");
      };

    loop(n, "");
  };

let main = () => {
  print_endline(repeat_x(-1));
  print_endline(repeat_x(0));
  print_endline(repeat_x(1));
  print_endline(repeat_x(2));
  print_endline(repeat_x(5));
};

let () = main();
