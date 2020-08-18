module type WATCHTOWER = {
  type hour = int;
  let zero: hour;
  let add: (hour, hour) => hour;
  let sub: (hour, hour) => hour;
};

module Watchtower: WATCHTOWER = {
  type hour = int;
  let zero = 12;
  let add = (h1, h2) => (h1 + h2 mod zero) mod zero;
  let sub = (h1, h2) => {
    let factor =
      if (h1 < h2 mod zero) {
        12;
      } else {
        0;
      };
    factor + (h1 - h2 mod zero) mod zero;
  };
};

let () = {
  let h1: Watchtower.hour = (6: Watchtower.hour);
  let h2: Watchtower.hour = (8: Watchtower.hour);
  let h1_plus_h2 = Watchtower.add(h1, h2);
  let h1_sub_h2 = Watchtower.sub(h1, h2);
  print_endline(string_of_int(h1_plus_h2));
  print_endline(string_of_int(h1_sub_h2));
  let h3: Watchtower.hour = (25: Watchtower.hour);
  let h1_plus_h3 = Watchtower.add(h1, h3);
  print_endline(string_of_int(h1_plus_h3));
  print_endline(string_of_int(Watchtower.sub(8, 4)));
  print_endline(string_of_int(Watchtower.sub(3, 4)));
  print_endline(string_of_int(Watchtower.sub(3, 26)));
};
