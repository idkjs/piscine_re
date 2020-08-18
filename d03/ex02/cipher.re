let rot42 = str => {
  let rot42_char = c =>
    switch (c) {
    | 'a'..'z' =>
      char_of_int(
        int_of_char('a') + (int_of_char(c) - int_of_char('a') + 42) mod 26,
      )
    | 'A'..'Z' =>
      char_of_int(
        int_of_char('A') + (int_of_char(c) - int_of_char('A') + 42) mod 26,
      )
    | _ => c
    };

  String.map(rot42_char, str);
};

let caesar = (n, str) => {
  let caesar_char = c =>
    switch (c) {
    | 'a'..'z' =>
      char_of_int(
        int_of_char('a') + (int_of_char(c) - int_of_char('a') + n) mod 26,
      )
    | 'A'..'Z' =>
      char_of_int(
        int_of_char('A') + (int_of_char(c) - int_of_char('A') + n) mod 26,
      )
    | _ => c
    };

  String.map(caesar_char, str);
};

let xor = (key: int, str: string) =>
  key > 0
    ? {
      let xor_char = c => char_of_int(int_of_char(c) lxor (key mod 256));
      String.map(xor_char, str);
    }
    : str;

let rec ft_crypt = (str: string, list_fct: list(string => string)) =>
  switch (list_fct) {
  | [] => str
  | [h, ...t] => ft_crypt(h(str), t)
  };
