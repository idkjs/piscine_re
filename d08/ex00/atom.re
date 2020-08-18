class virtual atom (name: string, symbol: string, number: int) = {
  pub name: string = name;
  pub symbol: string = symbol;
  pub atomic_number: int = number;
  pub to_string: string =
    "Atom of "
    ++ this#name
    ++ " with symbol "
    ++ this#symbol
    ++ " and atomic number "
    ++ string_of_int(this#atomic_number);
  pub equals = (that: atom) =>
    this#name == that#name
    && this#symbol == that#symbol
    && this#atomic_number == that#atomic_number;
};

class hydrogen = {
  as _;
  inherit (class atom)("Hydrogen", "H", 1);
};

class carbon = {
  as _;
  inherit (class atom)("Carbon", "C", 6);
};

class oxygen = {
  as _;
  inherit (class atom)("Oxygen", "O", 8);
};

class helium = {
  as _;
  inherit (class atom)("Helium", "He", 2);
};

class calcium = {
  as _;
  inherit (class atom)("Calcium", "Ca", 20);
};

class nitrogen = {
  as _;
  inherit (class atom)("Nitrogen", "N", 7);
};

class aluminium = {
  as _;
  inherit (class atom)("Aluminium", "Al", 13);
};
