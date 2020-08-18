exception Unbalanced(string);

let rec getOccurences = (lst: list('a), atom: 'a, n: int) =>
  switch (lst) {
  | [] => n
  | [h, ...t] when atom#equals(h) => getOccurences(t, atom, n + 1)
  | [h, ...t] => getOccurences(t, atom, n)
  };

let count_atoms =
    (pair_mol_count: list((Molecule.molecule, int))): list((string, int)) => {
  let rec list_atoms_occur =
          (
            fullLst: list(Atom.atom),
            lst: list(Atom.atom),
            uniques: list(Atom.atom),
            res: list((string, int)),
          ) =>
    switch (lst) {
    | [] => res
    | [h, ...t] when List.exists(x => h#equals(x), uniques) =>
      list_atoms_occur(fullLst, t, uniques, res)
    | [h, ...t] =>
      list_atoms_occur(
        fullLst,
        t,
        [h, ...uniques],
        [(h#getSymbol, getOccurences(fullLst, h, 0)), ...res],
      )
    };

  let rec flat_loop = (lst: list(Atom.atom), n: int, res: list(Atom.atom)) =>
    switch (n) {
    | i when i == 0 => res
    | _ => flat_loop(lst, n - 1, res @ lst)
    };

  let break_mol =
    List.map(((mol, count)) => (mol#getAtoms, count), pair_mol_count);
  let flattened =
    List.concat(List.map(((l, n)) => flat_loop(l, n, []), break_mol));
  list_atoms_occur(flattened, flattened, [], []);
};

let compare_list_atoms =
    (react: list((string, int)), prod: list((string, int))) => {
  let rec loop = lst_react =>
    switch (lst_react) {
    | [] => true
    | [(symbol, count), ...t] =>
      let count_in_prod = List.assoc_opt(symbol, prod);
      if (count_in_prod == None) {
        false;
      } else if (Option.get(count_in_prod) != count) {
        false;
      } else {
        loop(t);
      };
    };

  loop(react);
};

let mol_occur_list =
    (molecules: list(Molecule.molecule)): list((Molecule.molecule, int)) => {
  let rec loop =
          (
            lst: list(Molecule.molecule),
            uniques: list(Molecule.molecule),
            res: list((Molecule.molecule, int)),
          ) =>
    switch (lst) {
    | [] => res
    | [h, ...t] when List.exists(x => h#equals(x), uniques) =>
      loop(t, uniques, res)
    | [h, ...t] =>
      loop(
        t,
        [h, ...uniques],
        [(h, getOccurences(molecules, h, 0)), ...res],
      )
    };
  loop(molecules, [], []);
};

let rec add_molecule =
        (lst: list((Molecule.molecule, int)), mol: Molecule.molecule, res) =>
  switch (lst) {
  | [] => res
  | [(h, count), ...t] when h#equals(mol) =>
    add_molecule(t, mol, res @ [(h, count + 1)])
  | [h, ...t] => add_molecule(t, mol, res @ [h])
  };

let count_indiv_atom = (atom: Atom.atom, molecule: Molecule.molecule) =>
  getOccurences(molecule#getAtoms, atom, 0);

let getCoeff =
    (
      reactives: list((Molecule.molecule, int)),
      products: list((Molecule.molecule, int)),
    ) => {
  let rec calc_coeff = n => {
    print_endline(string_of_int(n));
    let p = count_indiv_atom(new Atom.carbon, fst(List.hd(reactives))) * n;
    print_endline(string_of_int(p));
    let q =
      count_indiv_atom(new Atom.hydrogen, fst(List.hd(reactives))) * n / 2;
    print_endline(string_of_int(q));
    if (q mod 2 == 0) {
      (n, p + q / 2, p, q);
    } else {
      calc_coeff(2 * n);
    };
  };
  calc_coeff(snd(List.hd(reactives)));
};

class alkane_combustion (alkanes: list(Alkane.alkane)) = {
  inherit
    (class Reaction.reaction)(
      (alkanes :> list(Molecule.molecule)) @ [new Molecule.dioxygen],
      [new Molecule.water, new Molecule.carbon_dioxyde],
    );
  val _reactives: list((Molecule.molecule, int)) =
    mol_occur_list(
      List.rev(
        (alkanes :> list(Molecule.molecule)) @ [new Molecule.dioxygen],
      ),
    );
  val _products: list((Molecule.molecule, int)) =
    mol_occur_list([new Molecule.water, new Molecule.carbon_dioxyde]);
  pub get_start: list((Molecule.molecule, int)) =
    switch (this#is_balanced) {
    | false => raise(Unbalanced("Error : The reaction is not balanced"))
    | true => _reactives
    };
  pub get_result: list((Molecule.molecule, int)) =
    switch (this#is_balanced) {
    | false => raise(Unbalanced("Error : The reaction is not balanced"))
    | true => _products
    };
  pub pair_react_prod: (
    list((Molecule.molecule, int)),
    list((Molecule.molecule, int)),
  ) = {
    let (n, m, p, q) = getCoeff(_reactives, _products);
    /* print_endline (string_of_int n) ; print_endline (string_of_int m) ; print_endline (string_of_int p) ; print_endline (string_of_int q) ; */
    let rec newReact = (lst_react, res_react) =>
      switch (lst_react) {
      | [] => res_react
      | [(mol, count), ...t] when mol#equals(new Molecule.dioxygen) =>
        newReact(t, res_react @ [(mol, m)])
      | [(mol, count), ...t] => newReact(t, res_react @ [(mol, n)])
      };

    let rec newProd = (lst_prod, res_prod) =>
      switch (lst_prod) {
      | [] => res_prod
      | [(mol, count), ...t] when mol#equals(new Molecule.carbon_dioxyde) =>
        newProd(t, res_prod @ [(mol, p)])
      | [(mol, count), ...t] => newProd(t, res_prod @ [(mol, q)])
      };

    (newReact(_reactives, []), newProd(_products, []));
  };
  pub balance: Reaction.reaction = {
    let (r, p) = this#pair_react_prod;
    ({<_reactives: r, _products: p>} :> Reaction.reaction);
  };
  pub is_balanced: bool = {
    let atoms_int_reactives: list((string, int)) = (
      count_atoms(_reactives): list((string, int))
    );
    let atoms_int_products: list((string, int)) = (
      count_atoms(_products): list((string, int))
    );
    compare_list_atoms(atoms_int_reactives, atoms_int_products);
  };
};
