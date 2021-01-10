let rec converges = (f, x, n) =>
  if (n === 0) {
    if (f(x) === x) {
      true;
    } else {
      false;
    };
  } else {
    converges(f, f(x), n - 1);
  };

let main = () => {
  if (converges(x => x * 2, 2, 4) === true) {
    print_endline("true");
  } else {
    print_endline("false");
  };
  if (converges(x => x + 1, 4, 6) === true) {
    print_endline("true");
  } else {
    print_endline("false");
  };
  if (converges(x => x / 2, 2, 3) === true) {
    print_endline("true");
  } else {
    print_endline("false");
  };
  if (converges(x => x / 2, 2, 2) === true) {
    print_endline("true");
  } else {
    print_endline("false");
  };
  if (converges(( * )(2), 2, 5) === true) {
    print_endline("true");
  } else {
    print_endline("false");
  };
};

let () = main();
