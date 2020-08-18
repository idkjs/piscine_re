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
