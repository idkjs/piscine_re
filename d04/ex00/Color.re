type t =
  | Spade
  | Heart
  | Diamond
  | Club;

let all = [Spade, Heart, Diamond, Club];

let toString = (color: t) =>
  switch (color) {
  | Spade => "S"
  | Heart => "H"
  | Diamond => "D"
  | Club => "C"
  };

let toStringVerbose = (color: t) =>
  switch (color) {
  | Spade => "Spade"
  | Heart => "Heart"
  | Diamond => "Diamond"
  | Club => "Club"
  };
