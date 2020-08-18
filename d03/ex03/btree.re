type tree('a) =
  | Nil
  | Node('a, tree('a), tree('a));

type intOption =
  | Some(int)
  | None;

let is_bst = (tree: tree(int)) => {
  let rec is_bst_left =
          (tree_left: tree('a), maxValue, min: tree('a), max: tree('a)) =>
    switch (tree_left) {
    | Nil => true
    | [@implicit_arity] Node(v, l, r) when v > maxValue => false
    | [@implicit_arity] Node(v, l, r) =>
      switch (min) {
      | [@implicit_arity] Node(minValue, _, _) when v < minValue => false
      | _ =>
        is_bst_left(l, v, min, tree_left)
        && is_bst_right(r, v, tree_left, max)
      }
    }
  and is_bst_right =
      (tree_right: tree('a), minValue, min: tree('a), max: tree('a)) =>
    switch (tree_right) {
    | Nil => true
    | [@implicit_arity] Node(v, l, r) when v < minValue => false
    | [@implicit_arity] Node(v, l, r) =>
      switch (max) {
      | [@implicit_arity] Node(maxValue, _, _) when v > maxValue => false
      | _ =>
        is_bst_left(l, v, min, tree_right)
        && is_bst_right(r, v, tree_right, max)
      }
    };

  switch (tree) {
  | Nil => true
  | [@implicit_arity] Node(v, l, r) =>
    is_bst_left(l, v, Nil, tree) && is_bst_right(r, v, tree, Nil)
  };
};

let max = (a, b) =>
  if (a < b) {
    b;
  } else {
    a;
  };
let abs = a =>
  if (a < 0) {
    - a;
  } else {
    a;
  };
let rec height = (node: tree('a)) =>
  switch (node) {
  | Nil => 0
  | [@implicit_arity] Node(_, l, r) => 1 + max(height(l), height(r))
  };

let is_perfect = (tree: tree('a)) =>
  if (is_bst(tree) == false) {
    false;
  } else {
    let rec is_perfect_loop = (node: tree('a)) =>
      switch (node) {
      | Nil => true
      | [@implicit_arity] Node(_, l, r) =>
        let height_left = height(l);
        let height_right = height(r);
        if (height_left != height_right) {
          false;
        } else {
          is_perfect_loop(l) && is_perfect_loop(r);
        };
      };

    is_perfect_loop(tree);
  };

let is_balanced = (tree: tree('a)) =>
  if (is_bst(tree) == false) {
    false;
  } else {
    let rec is_balanced_loop = (node: tree('a)) =>
      switch (node) {
      | Nil => true
      | [@implicit_arity] Node(_, l, r) =>
        if (is_balanced_loop(l) == false || is_balanced_loop(r) == false) {
          false;
        } else {
          let height_left = height(l);
          let height_right = height(r);
          if (abs(height_left - height_right) > 1) {
            false;
          } else {
            true;
          };
        }
      };

    is_balanced_loop(tree);
  };

let rec search_bst = (value, bst: tree('a)) =>
  switch (bst) {
  | Nil => false
  | [@implicit_arity] Node(v, l, r) =>
    if (value == v) {
      true;
    } else if (value > v) {
      search_bst(value, r);
    } else {
      search_bst(value, l);
    }
  };

let add_bst = (value: 'a, tree: tree('a)) =>
  if (search_bst(value, tree)) {
    tree;
  } else {
    let rec add_bst_aux = (current_node: tree('a)) =>
      switch (current_node) {
      | Nil => [@implicit_arity] Node(value, Nil, Nil)
      | [@implicit_arity] Node(v, l, r) =>
        if (value < v) {
          [@implicit_arity] Node(v, add_bst_aux(l), r);
        } else {
          [@implicit_arity] Node(v, l, add_bst_aux(r));
        }
      };

    add_bst_aux(tree);
  };

let delete_bst = (value: 'a, tree: tree('a)) =>
  if (!search_bst(value, tree)) {
    tree;
  } else {
    let rec minValue = node =>
      switch (node) {
      | [@implicit_arity] Node(v, Nil, r) => v
      | [@implicit_arity] Node(v, l, r) => minValue(l)
      | Nil =>
        failwith(
          "Error : function minValue should not be called on empty tree",
        )
      };

    let rec delete_bst_aux = (valueToDelete, current_node: tree('a)) =>
      switch (current_node) {
      | Nil => current_node
      | [@implicit_arity] Node(v, l, r) when valueToDelete < v =>
        [@implicit_arity] Node(v, delete_bst_aux(valueToDelete, l), r)
      | [@implicit_arity] Node(v, l, r) when valueToDelete > v =>
        [@implicit_arity] Node(v, l, delete_bst_aux(valueToDelete, r))
      | [@implicit_arity] Node(v, l, r) when v === valueToDelete =>
        if (height(current_node) == 1) {
          Nil;
        } else if (l == Nil) {
          r;
        } else if (r == Nil) {
          l;
        } else {
          let min = minValue(r);
          [@implicit_arity] Node(min, l, delete_bst_aux(min, r));
        }
      | _ => failwith("Error in patttern matching delete_bst")
      };

    delete_bst_aux(value, tree);
  };

/* ******************** To print tree ************** */
let draw_tree = (tree: tree('a)) => {
  let draw_square = (x, y, size) =>
    if (size > 0) {
      Graphics.moveto(x - size / 2, y - size / 2);
      Graphics.lineto(x - size / 2, y + size / 2);
      Graphics.lineto(x + size / 2, y + size / 2);
      Graphics.lineto(x + size / 2, y - size / 2);
      Graphics.lineto(x - size / 2, y - size / 2);
    };

  let height = height(tree);
  let rec draw_tree_aux = (current_node, x, y, size, factor) =>
    switch (current_node) {
    | [@implicit_arity] Node(v, l, r) =>
      draw_square(x, y, size);
      Graphics.moveto(x - size / 5, y - size / 5);
      Graphics.draw_string(string_of_int(v));
      draw_tree_aux(l, x - size * factor, y - size * 3, size, factor - 1);
      Graphics.moveto(x, y - size / 2);
      Graphics.lineto(x - size * factor, y - (size * 3 - size / 2));
      draw_tree_aux(r, x + size * factor, y - size * 3, size, factor - 1);
      Graphics.moveto(x, y - size / 2);
      Graphics.lineto(x + size * factor, y - (size * 3 - size / 2));
    | Nil =>
      draw_square(x, y, size);
      Graphics.moveto(x - size / 5, y - size / 5);
      Graphics.draw_string("Nil");
    };

  draw_tree_aux(tree, 900, 1200, 50, height + 2);
};

/* ************************* *************** *********************************** */

let main = () => {
  let tree1 =
    [@implicit_arity]
    Node(
      8,
      [@implicit_arity]
      Node(
        6,
        [@implicit_arity] Node(1, Nil, Nil),
        [@implicit_arity] Node(7, [@implicit_arity] Node(6, Nil, Nil), Nil),
      ),
      [@implicit_arity]
      Node(
        10,
        [@implicit_arity] Node(9, Nil, Nil),
        [@implicit_arity]
        Node(12, [@implicit_arity] Node(11, Nil, Nil), Nil),
      ),
    );
  let tree2 =
    [@implicit_arity]
    Node(
      12,
      [@implicit_arity]
      Node(
        9,
        [@implicit_arity] Node(1, Nil, Nil),
        [@implicit_arity]
        Node(11, [@implicit_arity] Node(10, Nil, Nil), Nil),
      ),
      [@implicit_arity]
      Node(
        16,
        [@implicit_arity] Node(13, Nil, Nil),
        [@implicit_arity]
        Node(20, [@implicit_arity] Node(17, Nil, Nil), Nil),
      ),
    );
  let tree3 =
    [@implicit_arity]
    Node(
      12,
      [@implicit_arity]
      Node(
        9,
        [@implicit_arity] Node(1, Nil, Nil),
        [@implicit_arity] Node(10, [@implicit_arity] Node(8, Nil, Nil), Nil),
      ),
      [@implicit_arity]
      Node(
        16,
        [@implicit_arity] Node(13, Nil, Nil),
        [@implicit_arity]
        Node(20, [@implicit_arity] Node(17, Nil, Nil), Nil),
      ),
    );
  let tree4 =
    [@implicit_arity]
    Node(
      12,
      [@implicit_arity]
      Node(
        9,
        [@implicit_arity] Node(1, Nil, Nil),
        [@implicit_arity]
        Node(11, [@implicit_arity] Node(10, Nil, Nil), Nil),
      ),
      [@implicit_arity]
      Node(
        16,
        [@implicit_arity] Node(13, Nil, Nil),
        [@implicit_arity]
        Node(18, [@implicit_arity] Node(15, Nil, Nil), Nil),
      ),
    );
  let tree0 =
    [@implicit_arity]
    Node(
      8,
      [@implicit_arity]
      Node(
        5,
        [@implicit_arity] Node(1, Nil, Nil),
        [@implicit_arity] Node(4, [@implicit_arity] Node(3, Nil, Nil), Nil),
      ),
      [@implicit_arity]
      Node(
        10,
        [@implicit_arity] Node(9, Nil, Nil),
        [@implicit_arity]
        Node(12, [@implicit_arity] Node(11, Nil, Nil), Nil),
      ),
    );
  let tree5 =
    [@implicit_arity]
    Node(
      8,
      [@implicit_arity]
      Node(
        6,
        [@implicit_arity] Node(1, Nil, Nil),
        [@implicit_arity] Node(7, [@implicit_arity] Node(6, Nil, Nil), Nil),
      ),
      [@implicit_arity]
      Node(
        10,
        [@implicit_arity] Node(7, Nil, Nil),
        [@implicit_arity]
        Node(12, [@implicit_arity] Node(11, Nil, Nil), Nil),
      ),
    );
  let tree6 = [@implicit_arity] Node(3, Nil, Nil);
  let tree7 =
    [@implicit_arity]
    Node(
      10,
      [@implicit_arity]
      Node(
        8,
        [@implicit_arity] Node(7, Nil, Nil),
        [@implicit_arity] Node(9, Nil, Nil),
      ),
      [@implicit_arity]
      Node(
        15,
        [@implicit_arity] Node(12, Nil, Nil),
        [@implicit_arity] Node(17, Nil, Nil),
      ),
    );
  let tree8 =
    [@implicit_arity]
    Node(
      10,
      [@implicit_arity]
      Node(
        8,
        [@implicit_arity] Node(7, Nil, Nil),
        [@implicit_arity] Node(9, Nil, Nil),
      ),
      [@implicit_arity]
      Node(
        15,
        [@implicit_arity] Node(12, Nil, Nil),
        [@implicit_arity]
        Node(17, [@implicit_arity] Node(16, Nil, Nil), Nil),
      ),
    );
  let tree9 =
    [@implicit_arity]
    Node(
      10,
      [@implicit_arity]
      Node(
        8,
        [@implicit_arity] Node(7, Nil, Nil),
        [@implicit_arity] Node(9, Nil, Nil),
      ),
      [@implicit_arity]
      Node(
        15,
        [@implicit_arity] Node(12, Nil, Nil),
        [@implicit_arity]
        Node(
          18,
          [@implicit_arity]
          Node(17, [@implicit_arity] Node(16, Nil, Nil), Nil),
          Nil,
        ),
      ),
    );
  let tree10 =
    [@implicit_arity]
    Node(
      10,
      [@implicit_arity]
      Node(
        5,
        [@implicit_arity] Node(2, Nil, Nil),
        [@implicit_arity]
        Node(
          8,
          [@implicit_arity]
          Node(6, [@implicit_arity] Node(7, Nil, Nil), Nil),
          Nil,
        ),
      ),
      [@implicit_arity]
      Node(
        14,
        [@implicit_arity]
        Node(
          12,
          [@implicit_arity] Node(11, Nil, Nil),
          [@implicit_arity] Node(13, Nil, Nil),
        ),
        [@implicit_arity] Node(18, Nil, Nil),
      ),
    );

  print_endline(" ******** is_bst ***********");
  /* true  */
  print_endline(string_of_bool(is_bst(tree1)));
  /* true  */
  print_endline(string_of_bool(is_bst(tree2)));
  /* false because of  8*/
  print_endline(string_of_bool(is_bst(tree3)));
  /* false because of 15 */
  print_endline(string_of_bool(is_bst(tree4)));
  /* false because of 4 */
  print_endline(string_of_bool(is_bst(tree0)));
  /* false because of 7 */
  print_endline(string_of_bool(is_bst(tree5)));
  print_endline(string_of_bool(is_bst(tree6)));
  print_endline(string_of_bool(is_bst(tree7)));
  print_endline(string_of_bool(is_bst(tree8)));
  print_endline(string_of_bool(is_bst(tree9)));
  print_char('\n');

  print_endline(" ******** is_perfect *********");
  print_endline(string_of_bool(is_perfect(tree1)));
  print_endline(string_of_bool(is_perfect(tree2)));
  print_endline(string_of_bool(is_perfect(tree3)));
  print_endline(string_of_bool(is_perfect(tree4)));
  print_endline(string_of_bool(is_perfect(tree0)));
  print_endline(string_of_bool(is_perfect(tree5)));
  print_endline(string_of_bool(is_perfect(tree6)));
  print_endline(string_of_bool(is_perfect(tree7)));
  print_endline(string_of_bool(is_perfect(tree8)));
  print_endline(string_of_bool(is_perfect(tree9)));
  print_char('\n');

  print_endline(" ******** is_balanced *********");
  /* for is_balanced criteria, check followin sites:
     https://towardsdatascience.com/self-balancing-binary-search-trees-101-fc4f51199e1d
     https://www.geeksforgeeks.org/how-to-determine-if-a-binary-tree-is-balanced/ */
  print_endline(string_of_bool(is_balanced(tree6)));
  print_endline(string_of_bool(is_balanced(tree7)));
  print_endline(string_of_bool(is_balanced(tree8)));
  print_endline(string_of_bool(is_balanced(tree9)));
  print_endline(string_of_bool(is_balanced(tree10)));
  print_char('\n');

  print_endline(" ******** search_bst *********");
  print_endline(string_of_bool(search_bst(7, tree1)));
  print_endline(string_of_bool(search_bst(5, tree1)));
  print_endline(string_of_bool(search_bst(17, tree2)));
  print_endline(string_of_bool(search_bst(8, tree2)));
  print_char('\n');

  print_endline(" ******** search_bst *********");
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree1);
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(add_bst(4, tree1));
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree2);
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(add_bst(15, tree2));
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree9);
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(delete_bst(7, tree9));
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree9);
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(delete_bst(15, tree9));
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree9);
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(delete_bst(10, tree9));
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree2);
  ignore(Graphics.read_key());
  Graphics.open_graph(" 2048x2048");
  draw_tree(delete_bst(11, tree2));
  ignore(Graphics.read_key());
};

let () = main();
