let getAlkaneName = (n: int) =>
  switch (n) {
  | 1 => "Methane"
  | 2 => "Ethane"
  | 3 => "Propane"
  | 4 => "Butane"
  | 5 => "Pentane"
  | 6 => "Hexane"
  | 7 => "Heptane"
  | 8 => "Octane"
  | 9 => "Nonane"
  | 10 => "Decane"
  | 11 => "Undecane"
  | 12 => "Dodecane"
  | _ => failwith("Error: not required to test with n > 12")
  };

class virtual alkane (n: int) = {
  inherit
    (class Molecule.molecule)(
      getAlkaneName(n),
      List.init(n, x => new Atom.carbon)
      @ List.init(2 * n + 2, x => new Atom.hydrogen),
    );
};

class methane = {
  as _;
  inherit (class alkane)(1);
};

class ethane = {
  as _;
  inherit (class alkane)(2);
};

class propane = {
  as _;
  inherit (class alkane)(3);
};

class octane = {
  as _;
  inherit (class alkane)(8);
};
