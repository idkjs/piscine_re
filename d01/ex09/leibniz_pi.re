let leibniz_pi = delta => {
  let pi = 4. *. atan(1.);
  let absolute = x =>
    if (x >= 0.) {
      x;
    } else {
      -. x;
    };
  if (delta < 0.) {
    (-1);
  } else {
    let rec pi_aux = (iter, pi_calc) =>
      if (absolute(pi -. pi_calc) <= delta) {
        iter;
      } else {
        pi_aux(
          iter + 1,
          pi_calc
          +. 4.
          *. (-1.)
          ** float_of_int(iter)
          /. (2. *. float_of_int(iter) +. 1.),
        );
      };

    pi_aux(0, 0.);
  };
};

let main = () => {
  print_int(leibniz_pi(1.));
  print_char('\n');
  print_int(leibniz_pi(0.5));
  print_char('\n');
  print_int(leibniz_pi(0.3));
  print_char('\n');
  print_int(leibniz_pi(0.01));
  print_char('\n');
  print_int(leibniz_pi(-0.2));
  print_char('\n');
};

let () = main();
