let unrot42 = str => {
  let unrot42_char = c =>
    switch (c) {
    | 'a'..'z' =>
      char_of_int(
        int_of_char('z')
        + (int_of_char(c) - int_of_char('a') - 42 + 1)
        mod 26,
      )
    | 'A'..'Z' =>
      char_of_int(
        int_of_char('Z')
        + (int_of_char(c) - int_of_char('A') - 42 + 1)
        mod 26,
      )
    | _ => c
    };

  String.map(unrot42_char, str);
};

let uncaesar = (str, ~arg1 as n) => {
  let uncaesar_char = c =>
    switch (c) {
    | 'a'..'z' =>
      if (int_of_char(c) - n mod 26 >= int_of_char('a')) {
        char_of_int(int_of_char(c) - n mod 26);
      } else {
        char_of_int(
          int_of_char('z')
          - (int_of_char('a') - (int_of_char(c) - n mod 26))
          + 1,
        );
      }
    | 'A'..'Z' =>
      if (int_of_char(c) - n mod 26 >= int_of_char('A')) {
        char_of_int(int_of_char(c) - n mod 26);
      } else {
        char_of_int(
          int_of_char('Z')
          - (int_of_char('A') - (int_of_char(c) - n mod 26))
          + 1,
        );
      }
    | _ => c
    };

  String.map(uncaesar_char, str);
};

let rec ft_uncrypt = (str: string, list_fct: list(string => string)) =>
  switch (list_fct) {
  | [] => str
  | [h, ...t] => ft_uncrypt(h(str), t)
  };
