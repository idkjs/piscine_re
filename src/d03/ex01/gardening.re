type tree('a) =
  | Nil
  | Node('a, tree('a), tree('a));

let rec size = (tree: tree('a)) =>
  switch (tree) {
  | Nil => 0
  | [@implicit_arity] Node(_, l, r) => 1 + size(l) + size(r)
  };

let height = (tree: tree('a)) => {
  let rec height_aux = (current_node, height_acc) =>
    switch (current_node) {
    | Nil => height_acc
    | [@implicit_arity] Node(v, l, r) =>
      let height_left = height_aux(l, height_acc + 1);
      let height_right = height_aux(r, height_acc + 1);
      if (height_left < height_right) {
        height_right;
      } else {
        height_left;
      };
    };

  height_aux(tree, 0);
};

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
      Graphics.draw_string(v);
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

let main = () => {
  let tree =
    [@implicit_arity]
    Node(
      "Node",
      [@implicit_arity]
      Node(
        "Node",
        [@implicit_arity]
        Node("Node", Nil, [@implicit_arity] Node("Leaf", Nil, Nil)),
        [@implicit_arity]
        Node(
          "Node",
          [@implicit_arity] Node("Leaf", Nil, Nil),
          [@implicit_arity] Node("Leaf", Nil, Nil),
        ),
      ),
      [@implicit_arity]
      Node("Node", Nil, [@implicit_arity] Node("Leaf", Nil, Nil)),
    );
  let tree1 =
    [@implicit_arity]
    Node(
      "Node",
      [@implicit_arity]
      Node(
        "Node",
        [@implicit_arity] Node("Leaf", Nil, Nil),
        [@implicit_arity]
        Node("Node", Nil, [@implicit_arity] Node("Leaf", Nil, Nil)),
      ),
      Nil,
    );
  let tree2 =
    [@implicit_arity]
    Node(
      "Node",
      [@implicit_arity]
      Node(
        "Node",
        [@implicit_arity]
        Node(
          "Node",
          [@implicit_arity]
          Node("Node", [@implicit_arity] Node("Leaf", Nil, Nil), Nil),
          Nil,
        ),
        Nil,
      ),
      [@implicit_arity]
      Node(
        "Node",
        [@implicit_arity]
        Node(
          "Node",
          [@implicit_arity] Node("Leaf", Nil, Nil),
          [@implicit_arity]
          Node("Node", [@implicit_arity] Node("Leaf", Nil, Nil), Nil),
        ),
        Nil,
      ),
    );
  print_endline("********* tree **********");
  print_string("size : ");
  print_int(size(tree));
  print_char('\n');
  print_string("height : ");
  print_int(height(tree));
  print_char('\n');
  print_endline("********* tree1 **********");
  print_string("size : ");
  print_int(size(tree1));
  print_char('\n');
  print_string("height : ");
  print_int(height(tree1));
  print_char('\n');
  print_endline("********* tree2 **********");
  print_string("size : ");
  print_int(size(tree2));
  print_char('\n');
  print_string("height : ");
  print_int(height(tree2));
  print_char('\n');
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree);
  ignore(Graphics.read_key());
};

let () = main();
