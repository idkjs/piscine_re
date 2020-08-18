let rec ft_countdown = n =>
  if (n <= 0) {
    print_int(0);
    print_char('\n');
  } else {
    print_int(n);
    print_char('\n');
    ft_countdown(n - 1);
  };

let main = () => {
  ft_countdown(9);
  ft_countdown(0);
  ft_countdown(-3);
};

let () = main();
