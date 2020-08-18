let rec print_mol_list = (lst: list((Molecule.molecule, int))) =>
  switch (lst) {
  | [(mol, count), n, ...t] when count == 1 =>
    Printf.printf("%s + ", mol#formula);
    print_mol_list([n, ...t]);
  | [(mol, count), n, ...t] =>
    Printf.printf("%d%s + ", count, mol#formula);
    print_mol_list([n, ...t]);
  | [(mol, count), ...t] when count == 1 => Printf.printf("%s", mol#formula)
  | [(mol, count), ...t] => Printf.printf("%d%s", count, mol#formula)
  | [] => ()
  };

let () = {
  let r = (new AlkaneCombustion.alkane_combustion)([new Alkane.methane]);
  print_endline(string_of_bool(r#is_balanced));
  let new_eq = r#balance;
  print_endline(string_of_bool(new_eq#is_balanced));
  print_mol_list(new_eq#get_start);
  print_string(" -> ");
  print_mol_list(new_eq#get_result);

  let e = (new AlkaneCombustion.alkane_combustion)([new Alkane.ethane]);
  print_endline(string_of_bool(e#is_balanced));
  let new_eq_e = e#balance;
  print_endline(string_of_bool(new_eq_e#is_balanced));
  print_mol_list(new_eq_e#get_start);
  print_string(" -> ");
  print_mol_list(new_eq_e#get_result);

  let e = (new AlkaneCombustion.alkane_combustion)([new Alkane.propane]);
  print_endline(string_of_bool(e#is_balanced));
  let new_eq_e = e#balance;
  print_endline(string_of_bool(new_eq_e#is_balanced));
  print_mol_list(new_eq_e#get_start);
  print_string(" -> ");
  print_mol_list(new_eq_e#get_result);
};
