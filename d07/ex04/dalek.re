let charGenerator = () => {
  Random.self_init();
  String.make(1, char_of_int(Random.int(26) + 97));
};

let stringGenerator = () => {
  Random.self_init();
  let x = Random.int(4);
  switch (x) {
  | 0 => "Explain! Explain!"
  | 1 => "Exterminate! Exterminate!"
  | 2 => "I obey!"
  | 3 => "You are the Doctor! You are the enemy of the Daleks!"
  | _ => "Error : Impossible!"
  };
};

class dalek = {
  as self;
  val _name: string =
    "Dalek"
    ++ {
      let rec loop = (x, res) =>
        switch (x) {
        | i when i == 3 => res
        | _ => loop(x + 1, charGenerator() ++ res)
        };
      loop(0, "");
    };
  val _hp: int = 100;
  val mutable _shield: bool = true;
  pub getName = _name;
  pub getHp = _hp;
  pub getShield = _shield;
  pub setShield = b => _shield = b;
  pub to_string =
    "Name : "
    ++ self#getName
    ++ "; HP : "
    ++ string_of_int(self#getHp)
    ++ "; Shield : "
    ++ string_of_bool(self#getShield);
  pub talk = print_endline(stringGenerator());
  pub exterminate = (people: People.people) => {
    people#die;
    let chgShield =
      if (self#getShield == true) {
        false;
      } else {
        true;
      };
    self#setShield(chgShield);
  };
  pub die = print_endline("Emergency Temporal Shift!");
};
