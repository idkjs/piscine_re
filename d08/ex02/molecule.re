let generate_formula = (atoms: list(Atom.atom)): string => {
  let rec getOccurences = (lst: list(Atom.atom), atom: Atom.atom, n: int) =>
    switch (lst) {
    | [] => n
    | [h, ...t] when atom#equals(h) => getOccurences(t, atom, n + 1)
    | [h, ...t] => getOccurences(t, atom, n)
    };

  let rec inter_list =
          (
            lst: list(Atom.atom),
            uniques: list(Atom.atom),
            res: list((string, int)),
          ) =>
    switch (lst) {
    | [] => res
    | [h, ...t] when List.exists(x => h#equals(x), uniques) =>
      inter_list(t, uniques, res)
    | [h, ...t] =>
      inter_list(
        t,
        [h, ...uniques],
        [(h#getSymbol, getOccurences(atoms, h, 0)), ...res],
      )
    };

  let rec split_list =
          (
            lst: list((string, int)),
            ch: list((string, int)),
            other: list((string, int)),
            c: int,
          ) =>
    switch (lst) {
    | [] =>
      if (c == 0) {
        List.sort((x, y) => String.compare(fst(x), fst(y)), ch @ other);
      } else {
        ch @ List.sort((x, y) => String.compare(fst(x), fst(y)), other);
      }
    | [h, ...t] when fst(h) == "C" => split_list(t, [h, ...ch], other, 1)
    | [h, ...t] when fst(h) == "H" => split_list(t, ch @ [h], other, c)
    | [h, ...t] => split_list(t, ch, [h, ...other], c)
    };

  let to_string = (fullLst: list((string, int))) => {
    let pair_to_string = ((x, y)) =>
      if (y != 1) {
        x ++ string_of_int(y);
      } else {
        x;
      };
    let rec loop = (lst: list((string, int)), res: string) =>
      switch (lst) {
      | [] => res
      | [h, ...t] => loop(t, res ++ pair_to_string(h))
      };
    loop(fullLst, "");
  };

  let intermediate_list = inter_list(atoms, [], []);
  let sorted_list = split_list(intermediate_list, [], [], 0);
  to_string(sorted_list);
};

class virtual molecule (name: string, atoms: list(Atom.atom)) = {
  pub name: string = name;
  pub formula: string = generate_formula(atoms);
  pub to_string =
    "Molecule's name : "
    ++ this#name
    ++ ", molecule's formula : "
    ++ this#formula;
  pub equals = (that: molecule) =>
    this#name == that#name && this#formula == that#formula;
};

class water = {
  as _;
  inherit
    (class molecule)(
      "Water",
      [new Atom.hydrogen, new Atom.oxygen, new Atom.hydrogen],
    );
};

class carbon_dioxyde = {
  as _;
  inherit
    (class molecule)(
      "Carbon dioxyde",
      [new Atom.oxygen, new Atom.oxygen, new Atom.carbon],
    );
};

class methane = {
  as _;
  inherit
    (class molecule)(
      "Methane",
      [
        new Atom.hydrogen,
        new Atom.hydrogen,
        new Atom.hydrogen,
        new Atom.hydrogen,
        new Atom.carbon,
      ],
    );
};

class nitrous_oxyde = {
  as _;
  inherit
    (class molecule)(
      "Nitrous oxyde",
      [new Atom.nitrogen, new Atom.oxygen, new Atom.nitrogen],
    );
};

class glucose = {
  as _;
  inherit
    (class molecule)(
      "Glucose",
      List.init(6, x => new Atom.oxygen)
      @ List.init(12, x => new Atom.hydrogen)
      @ List.init(6, x => new Atom.carbon),
    );
};

class aluminium_oxyde = {
  as _;
  inherit
    (class molecule)(
      "Aluminium oxyde",
      [
        new Atom.oxygen,
        new Atom.oxygen,
        new Atom.aluminium,
        new Atom.oxygen,
        new Atom.aluminium,
      ],
    );
};
