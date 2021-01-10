let ft_print_rev = str => {
  let start = String.length(str) - 1;
  let rec loop = current_idx =>
    if (current_idx >= 0) {
      print_char(str.[current_idx]);
      loop(current_idx - 1);
    };

  loop(start);
  print_char('\n');
};

let main = () => {
  ft_print_rev("Hello World !");
  ft_print_rev("");
  ft_print_rev("0123456789");
};

let () = main();
