class people (name) = {
  as self;
  val _name: string = name;
  val _hp: int = 100;
  pub get_name = _name;
  pub get_hp = _hp;
  pub to_string =
    "Name : " ++ self#get_name ++ " / Hp = " ++ string_of_int(self#get_hp);
  pub talk =
    print_endline("I'm " ++ self#get_name ++ "! Do you know the Doctor?");
  pub die = print_endline("Aaaarghh!");
  initializer (
    print_endline(
      "People named "
      ++ self#get_name
      ++ " has been created with a default hp of 100",
    )
  );
};
