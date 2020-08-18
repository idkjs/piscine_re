module type FRACTIONAL_BITS = {let bits: int;};

module type FIXED = {
  type t;
  let of_float: float => t;
  let of_int: int => t;
  let to_float: t => float;
  let to_int: t => int;
  let to_string: t => string;
  let zero: t;
  let one: t;
  let succ: t => t;
  let pred: t => t;
  let min: (t, t) => t;
  let max: (t, t) => t;
  let gth: (t, t) => bool;
  let lth: (t, t) => bool;
  let gte: (t, t) => bool;
  let lte: (t, t) => bool;
  let eqp: (t, t) => bool; /* physical equality */
  let eqs: (t, t) => bool; /* structural equality */
  let add: (t, t) => t;
  let sub: (t, t) => t;
  let mul: (t, t) => t;
  let div: (t, t) => t;
  let foreach: (t, t, t => unit) => unit;
};

module type MAKE = (Input: FRACTIONAL_BITS) => FIXED;

module Make: MAKE =
  (Input: FRACTIONAL_BITS) => {
    type t = int;
    let bits = Input.bits;

    let of_int = (n: int): t => n lsl bits;
    let to_int = (t: t): int => t lsr bits;
    let of_float = (f: float): t =>
      int_of_float(floor(0.5 +. f *. float_of_int(of_int(1))));
    let to_float = (t: t): float =>
      float_of_int(t) /. float_of_int(of_int(1));
    let to_string = (t: t): string => {
      let tfl = to_float(t);
      let tint = to_int(t);
      if (tfl == 0. || tfl /. float_of_int(tint) == 1.0) {
        string_of_int(tint);
      } else {
        string_of_float(tfl);
      };
    };
    let zero = of_int(0);
    let one = of_int(1);
    let succ = t => t + 1;
    let pred = t => t - 1;
    let min = (t1, t2) =>
      if (t1 <= t2) {
        t1;
      } else {
        t2;
      };
    let max = (t1, t2) =>
      if (t1 >= t2) {
        t1;
      } else {
        t2;
      };
    let gth = (t1, t2) =>
      if (t1 > t2) {
        true;
      } else {
        false;
      };
    let lth = (t1, t2) =>
      if (t1 < t2) {
        true;
      } else {
        false;
      };
    let gte = (t1, t2) =>
      if (t1 >= t2) {
        true;
      } else {
        false;
      };
    let lte = (t1, t2) =>
      if (t1 <= t2) {
        true;
      } else {
        false;
      };
    let eqp = (t1, t2) =>
      if (t1 === t2) {
        true;
      } else {
        false;
      }; /* physical equality */
    let eqs = (t1, t2) =>
      if (t1 == t2) {
        true;
      } else {
        false;
      }; /* structural equality */
    let add = (t1, t2) => t1 + t2;
    let sub = (t1, t2) => t1 - t2;
    let mul = (t1, t2) => t1 * t2;
    let div = (t1, t2) => t1 / t2;
    let rec foreach = (st, ed, f) =>
      switch (st) {
      | x when eqs(x, ed) => f(st)
      | _ =>
        f(st);
        foreach(succ(st), ed, f);
      };
  };

module Fixed4: FIXED =
  Make({
    let bits = 4;
  });

module Fixed8: FIXED =
  Make({
    let bits = 8;
  });

let () = {
  let x8 = Fixed8.of_float(21.10);
  let y8 = Fixed8.of_float(21.32);
  let r8 = Fixed8.add(x8, y8);
  print_endline(Fixed8.to_string(r8));
  Fixed4.foreach(Fixed4.zero, Fixed4.one, f =>
    print_endline(Fixed4.to_string(f))
  );

  print_endline("");
  let a = Fixed8.of_int(21);
  print_string("of_int 21 = ");
  print_int(Fixed8.to_int(a));
  print_endline("");
  print_string("to_string 21 : ");
  print_string(Fixed8.to_string(a));
  print_endline("");
  print_string("zero = ");
  print_string(Fixed8.to_string(Fixed8.zero));
  print_endline("");
  print_string("one = ");
  print_string(Fixed8.to_string(Fixed8.one));
  print_endline("");
  print_string("succ of 1 = ");
  print_string(Fixed8.to_string(Fixed8.succ(Fixed8.one)));
  print_endline("");
  print_string("pred of 1 = ");
  print_string(Fixed8.to_string(Fixed8.pred(Fixed8.one)));
  print_endline("");
  let b = Fixed8.of_int(42);
  print_string("Max of 21 | 42 = ");
  print_string(Fixed8.to_string(Fixed8.max(a, b)));
  print_endline("");
  print_string("Min of 21 | 42 = ");
  print_string(Fixed8.to_string(Fixed8.min(a, b)));
  print_endline("");
  let test = t =>
    if (t) {
      print_endline("True");
    } else {
      print_endline("False");
    };

  print_string("gth : 42 > 21 ? ");
  test(Fixed8.gth(b, a));
  print_string("gth : 21 > 42 ? ");
  test(Fixed8.gth(a, b));
  print_string("gth : 1 > 1 ? ");
  test(Fixed8.gth(Fixed8.one, Fixed8.one));
  print_string("lth : 42 < 21 ? ");
  test(Fixed8.lth(b, a));
  print_string("lth : 21 < 42 ? ");
  test(Fixed8.lth(a, b));
  print_string("lth : 1 < 1 ? ");
  test(Fixed8.lth(Fixed8.one, Fixed8.one));
  print_string("gte : 42 >= 21 ? ");
  test(Fixed8.gte(b, a));
  print_string("gte : 21 >= 42 ? ");
  test(Fixed8.gte(a, b));
  print_string("gte : 1 >= 1 ? ");
  test(Fixed8.gte(Fixed8.one, Fixed8.one));
  print_string("lte : 42 <= 21 ? ");
  test(Fixed8.lte(b, a));
  print_string("lte : 21 <= 42 ? ");
  test(Fixed8.lte(a, b));
  print_string("lte : 1 <= 1 ? ");
  test(Fixed8.lte(Fixed8.one, Fixed8.one));
  print_string("eqp : 0 = 0 ? ");
  test(Fixed8.eqp(Fixed8.zero, Fixed8.zero));
  print_string("eqp : 0 = 1 ? ");
  test(Fixed8.eqp(Fixed8.zero, Fixed8.one));
  print_string("eqs : 0 == 0 ? ");
  test(Fixed8.eqs(Fixed8.zero, Fixed8.zero));
  print_string("eqs : 0 == 1 ? ");
  test(Fixed8.eqs(Fixed8.zero, Fixed8.one));
  print_string("add : 21 + 42 = ");
  print_string(Fixed8.to_string(Fixed8.add(a, b)));
  print_endline("");
  print_string("sub : 21 - 42 = ");
  print_string(Fixed8.to_string(Fixed8.sub(a, b)));
  print_endline("");
  print_string("sub : 42 - 21 = ");
  print_string(Fixed8.to_string(Fixed8.sub(b, a)));
  print_endline("");
  print_string("mul : 21 * 42 = ");
  print_string(Fixed8.to_string(Fixed8.mul(a, b)));
  print_endline("");
  print_string("div : 42 / 21 = ");
  print_string(Fixed8.to_string(Fixed8.div(b, a)));
  print_endline("");
};
