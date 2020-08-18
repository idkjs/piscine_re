let repeat_string = (~str="x", n) =>
  if (n < 0) {
    "Error";
  } else {
    let rec loop = (x, acc) =>
      if (x === 0) {
        acc;
      } else {
        loop(x - 1, acc ++ str);
      };

    loop(n, "");
  };

let main = () => {
  print_endline(repeat_string(-1));
  print_endline(repeat_string(0));
  print_endline(repeat_string(~str="Toto", 1));
  print_endline(repeat_string(2));
  print_endline(repeat_string(~str="a", 5));
  print_endline(repeat_string(~str="what", 3));
};

let () = main();
