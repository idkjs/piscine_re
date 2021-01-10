let ft_string_all = (func, str) => {
  let len = String.length(str);
  let rec apply = current_idx =>
    if (current_idx === len) {
      true;
    } else {
      let current_char = str.[current_idx];
      if (!func(current_char)) {
        false;
      } else {
        apply(current_idx + 1);
      };
    };

  apply(0);
};

let is_digit = c => c >= '0' && c <= '9';

let main = () => {
  let a = ft_string_all(is_digit, "0123456789");
  let b = ft_string_all(is_digit, "0123e56789");
  if (a) {
    print_endline("true");
  } else {
    print_endline("false");
  };
  if (b) {
    print_endline("true");
  } else {
    print_endline("false");
  };
};

let () = main();
