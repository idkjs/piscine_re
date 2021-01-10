let rec printList = (listCards: list(Card.t)) =>
  switch (listCards) {
  | [] => print_char('\n')
  | [h, ...t] =>
    print_endline(Card.toString(h));
    print_endline(Card.toStringVerbose(h));
    printList(t);
  };

let shuffle = d => {
  let nd = List.map(c => (Random.bits(), c), d);
  let sond = List.sort(compare, nd);
  List.map(snd, sond);
};

let main = () => {
  printList(Card.allSpades);
  printList(Card.allHearts);
  printList(Card.allDiamonds);
  printList(Card.allClubs);
  printList(Card.all);
  print_char('\n');

  let newCard = Card.newCard(Card.Value.Jack, Card.Color.Diamond);
  print_endline(Card.Value.toStringVerbose(Card.getValue(newCard)));
  print_endline(Card.Color.toStringVerbose(Card.getColor(newCard)));
  print_char('\n');

  let newCard2 = Card.newCard(Card.Value.T8, Card.Color.Spade);
  print_int(Card.compare(newCard, newCard2));
  print_char('\n');
  print_endline(Card.toStringVerbose(Card.max(newCard, newCard2)));
  print_endline(Card.toStringVerbose(Card.min(newCard, newCard2)));
  print_char('\n');

  let shuffledList =
    shuffle(Card.allSpades)
    @ [Card.newCard(Card.Value.As, Card.Color.Diamond)];
  printList(shuffledList);
  print_endline(Card.toStringVerbose(Card.best(shuffledList)));
  print_char('\n');

  print_endline(string_of_bool(Card.isOf(newCard, Card.Color.Diamond)));
  print_endline(string_of_bool(Card.isOf(newCard, Card.Color.Spade)));
  print_endline(string_of_bool(Card.isDiamond(newCard)));
  print_endline(string_of_bool(Card.isSpade(newCard)));
};

let () = main();
