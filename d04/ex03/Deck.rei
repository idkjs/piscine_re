module Card: {
  module Color: {
    type t =
      | Spade
      | Heart
      | Diamond
      | Club;

    let all: list(t);

    let toString: t => string;
    let toStringVerbose: t => string;
  };

  module Value: {
    type t =
      | T2
      | T3
      | T4
      | T5
      | T6
      | T7
      | T8
      | T9
      | T10
      | Jack
      | Queen
      | King
      | As;
    let all: list(t);

    let toInt: t => int;
    let toString: t => string;
    let toStringVerbose: t => string;

    let next: t => t;
    let previous: t => t;
  };

  type t;

  let newCard: (Value.t, Color.t) => t;

  let allSpades: list(t);
  let allHearts: list(t);
  let allDiamonds: list(t);
  let allClubs: list(t);
  let all: list(t);

  let getValue: t => Value.t;
  let getColor: t => Color.t;

  let toString: t => string;
  let toStringVerbose: t => string;

  let compare: (t, t) => int;
  let max: (t, t) => t;
  let min: (t, t) => t;
  let best: list(t) => t;

  let isOf: (t, Color.t) => bool;
  let isSpade: t => bool;
  let isHeart: t => bool;
  let isDiamond: t => bool;
  let isClub: t => bool;
};

type t;

let newDeck: unit => t;

let toStringList: t => list(string);
let toStringListVerbose: t => list(string);

let drawCard: t => (Card.t, t);
