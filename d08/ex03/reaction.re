class virtual
      reaction
      (
        reactive: list((Molecule.molecule, int)),
        products: list((Molecule.molecule, int)),
      ) = {
  pub virtual get_start: list((Molecule.molecule, int));
  pub virtual get_result: list((Molecule.molecule, int));
  pub virtual balance: reaction;
  pub virtual is_balanced: bool;
};
