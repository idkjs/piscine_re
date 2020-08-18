let printListString = (listString: list(string)) =>
  switch (listString) {
  | [] => print_string("[]")
  | _ =>
    print_char('[');
    let rec print_aux = lst =>
      switch (lst) {
      | [] => print_endline("]")
      | [h, ...t] =>
        print_string(h);
        print_string(", ");
        print_aux(t);
      };

    print_aux(listString);
  };

let printCouple = (couple: (Deck.Card.t, Deck.t)) =>
  switch (couple) {
  | (h, t) =>
    Printf.printf("(%s, ", Deck.Card.toStringVerbose(h));
    printListString(Deck.toStringListVerbose(t));
    print_endline(")\n");
  };

let main = () => {
  let newDeck = Deck.newDeck();
  printListString(Deck.toStringList(newDeck));
  print_char('\n');
  print_char('\n');
  let newDeck2 = Deck.newDeck();
  printListString(Deck.toStringList(newDeck2));
  print_char('\n');
  print_char('\n');

  printCouple(Deck.drawCard(newDeck2));
};

let () = main();
