let print_numbers = (a, b) => {
  if (a / 10 === 0) {
    print_char('0');
    print_int(a);
  } else {
    print_int(a);
  };
  print_char(' ');
  if (b / 10 === 0) {
    print_char('0');
    print_int(b);
  } else {
    print_int(b);
  };
  if (!(a == 98 && b == 99)) {
    print_char(',');
    print_char(' ');
  };
};

let ft_print_comb2 = () => {
  let rec loop = (a, b) => {
    if (a <= 98) {
      if (b <= 99 && b > a) {
        print_numbers(a, b);
      };
    };
    if (b < 99) {
      loop(a, b + 1);
    } else if (b === 99) {
      loop(a + 1, a + 2);
    };
  };

  loop(0, 1);
  print_char('\n');
};

let main = () => ft_print_comb2();

let () = main();
