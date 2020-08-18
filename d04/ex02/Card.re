module Color = {
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
};

module Value = {
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

  let all = [T2, T3, T4, T5, T6, T7, T8, T9, T10, Jack, Queen, King, As];

  let toInt = (card: t) =>
    switch (card) {
    | T2 => 1
    | T3 => 2
    | T4 => 3
    | T5 => 4
    | T6 => 5
    | T7 => 6
    | T8 => 7
    | T9 => 8
    | T10 => 9
    | Jack => 10
    | Queen => 11
    | King => 12
    | As => 13
    };

  let toString = (card: t) =>
    switch (card) {
    | T2 => "2"
    | T3 => "3"
    | T4 => "4"
    | T5 => "5"
    | T6 => "6"
    | T7 => "7"
    | T8 => "8"
    | T9 => "9"
    | T10 => "10"
    | Jack => "J"
    | Queen => "Q"
    | King => "K"
    | As => "A"
    };

  let toStringVerbose = (card: t) =>
    switch (card) {
    | T2 => "2"
    | T3 => "3"
    | T4 => "4"
    | T5 => "5"
    | T6 => "6"
    | T7 => "7"
    | T8 => "8"
    | T9 => "9"
    | T10 => "10"
    | Jack => "Jack"
    | Queen => "Queen"
    | King => "King"
    | As => "As"
    };

  let next = (card: t) =>
    switch (card) {
    | As => invalid_arg("Error : provided card is As. No next card.")
    | _ =>
      let rec nextInAll = (l: list(t)) =>
        switch (l) {
        | [] => invalid_arg("Error: card not found")
        | [h, n, ...t] when h == card => n
        | [h, ...t] => nextInAll(t)
        };

      nextInAll(all);
    };

  let previous = (card: t) =>
    switch (card) {
    | T2 => invalid_arg("Error : provided card is T2. No previous card.")
    | _ =>
      let rec previousInAll = (l: list(t)) =>
        switch (l) {
        | [] => invalid_arg("Error: card not found")
        | [h, n, ...t] when n == card => h
        | [h, ...t] => previousInAll(t)
        };

      previousInAll(all);
    };
};

type t = {
  value: Value.t,
  color: Color.t,
};

let newCard = (v: Value.t, c: Color.t) => {value: v, color: c};

let allSpades = {
  let rec createAllSpades = (valueList: list(Value.t), result: list(t)) =>
    switch (valueList) {
    | [] => List.rev(result)
    | [h, ...t] => createAllSpades(t, [newCard(h, Color.Spade), ...result])
    };

  createAllSpades(Value.all, []);
};

let allHearts = {
  let rec createAllHearts = (valueList: list(Value.t), result: list(t)) =>
    switch (valueList) {
    | [] => List.rev(result)
    | [h, ...t] => createAllHearts(t, [newCard(h, Color.Heart), ...result])
    };

  createAllHearts(Value.all, []);
};

let allDiamonds = {
  let rec createAllDiamonds = (valueList: list(Value.t), result: list(t)) =>
    switch (valueList) {
    | [] => List.rev(result)
    | [h, ...t] =>
      createAllDiamonds(t, [newCard(h, Color.Diamond), ...result])
    };

  createAllDiamonds(Value.all, []);
};

let allClubs = {
  let rec createAllClubs = (valueList: list(Value.t), result: list(t)) =>
    switch (valueList) {
    | [] => List.rev(result)
    | [h, ...t] => createAllClubs(t, [newCard(h, Color.Club), ...result])
    };

  createAllClubs(Value.all, []);
};

let all = allSpades @ allHearts @ allDiamonds @ allClubs;

let getValue = (card: t) => card.value;
let getColor = (card: t) => card.color;

let toString = (card: t) =>
  Printf.sprintf(
    "%s%s",
    Value.toString(card.value),
    Color.toString(card.color),
  );

let toStringVerbose = (card: t) =>
  Printf.sprintf(
    "Card(%s, %s)",
    Value.toStringVerbose(card.value),
    Color.toStringVerbose(card.color),
  );

let compare = (c1: t, c2: t) =>
  Value.toInt(c1.value) - Value.toInt(c2.value);

let max = (c1: t, c2: t) =>
  if (Value.toInt(c1.value) >= Value.toInt(c2.value)) {
    c1;
  } else {
    c2;
  };
let min = (c1: t, c2: t) =>
  if (Value.toInt(c1.value) <= Value.toInt(c2.value)) {
    c1;
  } else {
    c2;
  };

let best = (cardList: list(t)) =>
  switch (cardList) {
  | [] => invalid_arg("Error: list of cards passed to best is empty.")
  | _ => List.fold_left(max, List.hd(cardList), List.tl(cardList))
  };

let isOf = (card: t, color: Color.t) => card.color == color;
let isSpade = (card: t) => card.color == Color.Spade;
let isHeart = (card: t) => card.color == Color.Heart;
let isDiamond = (card: t) => card.color == Color.Diamond;
let isClub = (card: t) => card.color == Color.Club;
