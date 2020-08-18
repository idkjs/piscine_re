class army ('a) (member: list('a)) = {
  as self;
  val mutable _member = member;
  pub add = (soldier: 'a) => _member = [soldier, ..._member];
  pub delete =
    _member = (
      try(List.tl(_member)) {
      | Failure(err) =>
        print_endline("Army is empty!");
        _member;
      }
    );
  pub armySize = List.length(_member);
  pub armyMembersString = {
    let rec loop = lst =>
      switch (lst) {
      | [] => print_char('\n')
      | [h, ...t] =>
        print_endline(h#to_string);
        loop(t);
      };
    loop(_member);
  };
  pub check_alive =
    switch (_member) {
    | [] => false
    | _ => true
    };
  pub get_fighter = (x: int) => List.nth(_member, x);
};
