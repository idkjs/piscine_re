module type SET = {
  type t('a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let union: (t('a), t('a)) => t('a);
  let inter: (t('a), t('a)) => t('a);
  let diff: (t('a), t('a)) => t('a);
  let filter: (t('a), 'a => bool) => t('a);
  let foreach: (t('a), 'a => unit) => unit;
  let for_all: (t('a), 'a => bool) => bool;
  let exists: (t('a), 'a => bool) => bool;
  let append: (t('a), 'a) => t('a);
  let concat: (t('a), t('a)) => t('a);
  let print_set_int: t(int) => unit;
};

module Set: SET = {
  type t('a) = list('a);
  let return = x => [x];
  let bind = (set, f) => List.concat(List.map(f, set));

  let union = (set_a, set_b) => {
    let combine_set = set_a @ set_b;
    let rec loop_unique = (set, res) =>
      switch (set) {
      | [] => res
      | [h, ...t] when List.find_opt(x => x == h, res) == None =>
        loop_unique(t, res @ [h])
      | [h, ...t] => loop_unique(t, res)
      };
    loop_unique(combine_set, []);
  };

  let inter = (set_a, set_b) =>
    List.filter(
      x => {
        let is_in_setb = List.find_opt(a => a == x, set_b);
        if (is_in_setb == None) {
          false;
        } else {
          true;
        };
      },
      set_a,
    );

  let diff = (set_a, set_b) => {
    let a_not_b =
      List.filter(
        x => {
          let is_not_in_setb = List.find_opt(a => a == x, set_b);
          if (is_not_in_setb == None) {
            true;
          } else {
            false;
          };
        },
        set_a,
      );
    let b_not_a =
      List.filter(
        x => {
          let is_not_in_seta = List.find_opt(a => a == x, set_a);
          if (is_not_in_seta == None) {
            true;
          } else {
            false;
          };
        },
        set_b,
      );
    a_not_b @ b_not_a;
  };

  let filter = (set, p) => List.filter(p, set);
  let foreach = (set, f) => List.iter(f, set);
  let for_all = (set, p) => List.for_all(p, set);
  let exists = (set, p) => List.exists(p, set);
  let append = (set, a) =>
    if (List.find_opt(x => x == a, set) == None) {
      set @ [a];
    } else {
      set;
    };
  let concat = (set1, set2) => union(set1, set2);

  let rec print_set_int = set =>
    switch (set) {
    | [] => print_char('\n')
    | [h, ...t] =>
      print_string(string_of_int(h) ++ " ");
      print_set_int(t);
    };
};

let () = {
  let set =
    Set.concat(
      Set.concat(
        Set.concat(Set.return(10), Set.return(5)),
        Set.return(8),
      ),
      Set.return(-5),
    );
  let set2 =
    Set.concat(
      Set.concat(
        Set.concat(Set.return(2), Set.return(10)),
        Set.return(0),
      ),
      Set.return(-1),
    );
  let set3 =
    Set.concat(
      Set.concat(
        Set.concat(Set.return(13), Set.return(5)),
        Set.return(-5),
      ),
      Set.return(3),
    );
  Set.print_set_int(set);
  Set.print_set_int(set2);
  Set.print_set_int(set3);
  Set.print_set_int(Set.union(Set.union(set, set2), set3));
  Set.print_set_int(Set.inter(set, set3));
  Set.print_set_int(Set.diff(set, set3));
  Set.print_set_int(Set.filter(set2, x => x > 0));
  Set.foreach(set, x => print_string(string_of_int(x * x) ++ " "));
  print_char('\n');
  print_endline(string_of_bool(Set.for_all(set3, x => x mod 2 != 0)));
  print_endline(string_of_bool(Set.for_all(set, x => x mod 2 != 0)));
  print_endline(string_of_bool(Set.exists(set, x => x mod 2 != 0)));
};
