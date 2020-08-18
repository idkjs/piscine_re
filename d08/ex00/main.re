let () = {
  let h = new Atom.hydrogen;
  let c = new Atom.carbon;
  let o = new Atom.oxygen;
  print_endline(h#to_string);
  print_endline(c#to_string);
  print_endline(o#to_string);
  let h2 = new Atom.hydrogen;
  print_endline(string_of_bool(h#equals(h2)));
  print_endline(string_of_bool(h#equals(c)));
  print_endline(string_of_bool(h#equals(o)));
};
