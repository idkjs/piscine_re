let draw_tardis = () =>
  print_endline(
    "\n            ___        \n \n    _______(_@_)_______\n\n    | POLICE      BOX |\n\n    |_________________|\n\n     | _____ | _____ | \n\n     | |###| | |###| | \n\n     | |###| | |###| | \n\n     | _____ | _____ | \n\n     | || || | || || | \n\n     | ||_|| | ||_|| | \n\n     | _____ |$_____ | \n\n     | || || | || || | \n\n     | ||_|| | ||_|| | \n\n     | _____ | _____ | \n\n     | || || | || || | \n\n     | ||_|| | ||_|| | \n\n     |       |       | \n\n     ***************** \n",
  );

class doctor (name: string, age: int, sidekick: People.people) = {
  as self;
  val _name = name;
  val mutable _age = age;
  val _sidekick = sidekick;
  val mutable _hp = 100;
  pub getName = _name;
  pub getAge = _age;
  pub getSidekick = _sidekick;
  pub getHp = _hp;
  pub setHp = x => _hp = x;
  pub to_string =
    "Doctor "
    ++ self#getName
    ++ ", aged "
    ++ string_of_int(self#getAge)
    ++ ", sidekick "
    ++ self#getSidekick#to_string
    ++ ", with hp "
    ++ string_of_int(self#getHp);
  pub talk = print_string("Hi! I’m the Doctor!");
  initializer (print_endline("Doctor named " ++ self#getName ++ " is born!"));
  pub travel_in_time = (start: int, arrival: int) => {
    _age = _age + start + arrival;
    draw_tardis();
  };
  pub use_sonic_screwdriver =
    print_string("Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii");
  pri regenerate = _hp = 100;
  pub test_regenerate = self#regenerate;
};
