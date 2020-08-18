let () = {
  let m = new Alkane.methane;
  let e = new Alkane.ethane;
  let o = new Alkane.octane;
  print_endline(m#to_string);
  print_endline(e#to_string);
  print_endline(o#to_string);
  let m2 = new Alkane.methane;
  print_endline(string_of_bool(m#equals(m2)));
  print_endline(string_of_bool(m#equals(o)));
};
