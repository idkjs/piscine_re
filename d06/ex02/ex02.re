module type PAIR = {let pair: (int, int);};

module type VAL = {let x: int;};

module type MAKEFSTSND = (Pair: PAIR) => VAL;

module MakeFst: MAKEFSTSND =
  (Pair: PAIR) => {
    let x = fst(Pair.pair);
  };

module MakeSnd: MAKEFSTSND =
  (Pair: PAIR) => {
    let x = snd(Pair.pair);
  };

module Pair: PAIR = {
  let pair = (21, 42);
};

module Fst: VAL = MakeFst(Pair);

module Snd: VAL = MakeSnd(Pair);

let () = Printf.printf("Fst.x = %d, Snd.x = %d\n", Fst.x, Snd.x);
