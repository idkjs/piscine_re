let () = {
  let w = new Molecule.water;
  let cd = new Molecule.carbon_dioxyde;
  let m = new Molecule.methane;
  let no = new Molecule.nitrous_oxyde;
  let g = new Molecule.glucose;
  let alo = new Molecule.aluminium_oxyde;
  print_endline(w#to_string);
  print_endline(cd#to_string);
  print_endline(m#to_string);
  print_endline(no#to_string);
  print_endline(g#to_string);
  print_endline(alo#to_string);
  let m2 = new Molecule.methane;
  print_endline(string_of_bool(w#equals(m)));
  print_endline(string_of_bool(m#equals(m2)));
};
