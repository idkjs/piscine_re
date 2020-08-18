module type MONOID = {
  type element;
  let zero1: element;
  let zero2: element;
  let mul: (element, element) => element;
  let add: (element, element) => element;
  let div: (element, element) => element;
  let sub: (element, element) => element;
};

module INT = {
  type element = int;
  let zero1 = 0;
  let zero2 = 1;
  let add = (+);
  let sub = (-);
  let mul = ( * );
  let div = (/);
};

module FLOAT = {
  type element = float;
  let zero1 = 0.;
  let zero2 = 1.;
  let add = (+.);
  let sub = (-.);
  let mul = ( *. );
  let div = (/.);
};

module type CALC =
  (M: MONOID) =>
   {
    let add: (M.element, M.element) => M.element;
    let sub: (M.element, M.element) => M.element;
    let mul: (M.element, M.element) => M.element;
    let div: (M.element, M.element) => M.element;
    let power: (M.element, int) => M.element;
    let fact: M.element => M.element;
  };

module Calc: CALC =
  (M: MONOID) => {
    let add = (x, y) => M.add(x, y);
    let sub = (x, y) => M.sub(x, y);
    let mul = (x, y) => M.mul(x, y);
    let div = (x, y) => M.div(x, y);

    let rec power = (x: M.element, y: int) =>
      switch (y) {
      | 0 => M.zero2
      | 1 => x
      | n when n mod 2 == 0 =>
        let a = power(x, n / 2);
        M.mul(a, a);
      | n =>
        let a = power(x, n / 2);
        M.mul(x, M.mul(a, a));
      };

    let rec fact = (x: M.element) =>
      switch (x) {
      | n when n == M.zero1 || n == M.zero2 => M.zero2
      | n => M.mul(n, fact(M.sub(n, M.zero2)))
      };
  };
