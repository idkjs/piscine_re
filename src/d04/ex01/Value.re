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
