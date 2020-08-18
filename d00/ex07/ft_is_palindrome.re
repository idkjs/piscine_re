let ft_is_palindrome = str => {
  let first = 0;
  let last = String.length(str) - 1;
  let rec loop = (first_idx, last_idx) =>
    if (last < 0 || first_idx >= last_idx) {
      true;
    } else {
      let first_char = str.[first_idx];
      let last_char = str.[last_idx];
      if (first_char != last_char) {
        false;
      } else {
        loop(first_idx + 1, last_idx - 1);
      };
    };

  loop(first, last);
};

let main = () => {
  let a = "madam";
  let b = "radar";
  let c = "";
  let d = "abcde";
  print_endline("-----------");
  print_endline(a);
  if (ft_is_palindrome(a)) {
    print_endline("true");
  } else {
    print_endline("false");
  };
  print_endline("-----------");
  print_endline(b);
  if (ft_is_palindrome(b)) {
    print_endline("true");
  } else {
    print_endline("false");
  };
  print_endline("-----------");
  print_endline(c);
  if (ft_is_palindrome(c)) {
    print_endline("true");
  } else {
    print_endline("false");
  };
  print_endline("-----------");
  print_endline(d);
  if (ft_is_palindrome(d)) {
    print_endline("true");
  } else {
    print_endline("false");
  };
};

let () = main();
