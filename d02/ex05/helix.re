type phosphate = string;
type deoxyribose = string;
type nucleobase =
  | A
  | T
  | C
  | G
  | None;

type nucleotide = {
  phosphate,
  deoxyribose,
  nucleobase,
};

type helix = list(nucleotide);

let generate_nucleotide = c => {
  phosphate: "phosphate",
  deoxyribose: "deoxyribose",
  nucleobase:
    switch (c) {
    | 'A' => A
    | 'T' => T
    | 'C' => C
    | 'G' => G
    | _ => None
    },
};

let generate_helix = n =>
  if (n < 1) {
    print_endline(
      "Error: number of nucleotides must be greater than 0 to generate an helix",
    );
    [];
  } else {
    Random.self_init();
    let rec choice =
      fun
      | 0 => 'A'
      | 1 => 'T'
      | 2 => 'C'
      | 3 => 'G'
      | _ => 'O';

    let rec gen_helix_aux = (i, acc: helix) =>
      switch (i) {
      | y when y == n => acc
      | _ =>
        gen_helix_aux(
          i + 1,
          [generate_nucleotide(choice(Random.int(4))), ...acc],
        )
      };

    gen_helix_aux(0, []);
  };

type strOption =
  | String(string)
  | None;
let extractStrOption = value =>
  switch (value) {
  | None => "N/A"
  | String(str) => str
  };

let helix_to_string = (lst: helix) => {
  let nucleobase_str =
    fun
    | A => String("A")
    | T => String("T")
    | C => String("C")
    | G => String("G")
    | _ => None;

  let rec loop = (lst_remaining, str) =>
    switch (lst_remaining) {
    | [] => str
    | [h, ...t] =>
      let nclbase = nucleobase_str(h.nucleobase);
      switch (nclbase) {
      | None => loop(t, str)
      | _ => loop(t, str ++ extractStrOption(nclbase))
      };
    };

  loop(lst, "");
};

let complementary_helix = (helix: helix) => {
  let get_complement =
    fun
    | A => 'T'
    | T => 'A'
    | C => 'G'
    | G => 'C'
    | _ => 'O';

  let rec loop = (remaining_helix, comp_helix: helix) =>
    switch (remaining_helix) {
    | [] => comp_helix
    | [h, ...t] =>
      loop(
        t,
        comp_helix @ [generate_nucleotide(get_complement(h.nucleobase))],
      )
    };

  loop(helix, []);
};

let () = {
  let hlx = generate_helix(5);
  print_endline(helix_to_string(hlx));
  print_endline(helix_to_string(complementary_helix(hlx)));
  print_endline("------------");
  let hlx1 = generate_helix(6);
  print_endline(helix_to_string(hlx1));
  print_endline(helix_to_string(complementary_helix(hlx1)));
  print_endline("------------");
  let hlx2 = generate_helix(7);
  print_endline(helix_to_string(hlx2));
  print_endline(helix_to_string(complementary_helix(hlx2)));
  print_endline("------------");
  let hlx3 = generate_helix(8);
  print_endline(helix_to_string(hlx3));
  print_endline(helix_to_string(complementary_helix(hlx3)));
  print_endline("------------");
  let hlx4 = generate_helix(0);
  print_endline(helix_to_string(hlx4));
};
