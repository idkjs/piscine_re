let () = {
  let people = (new People.people)("Xoriman");
  let doctor = (new Doctor.doctor)("Who", 42, people);
  let newDalek = new Dalek.dalek;
  print_endline(newDalek#to_string);
  newDalek#talk;
  print_char('\n');
  newDalek#exterminate(people);
  print_char('\n');
  print_endline(newDalek#to_string);
  newDalek#talk;
  print_char('\n');
  doctor#use_sonic_screwdriver;
  print_char('\n');
  newDalek#die;
};
