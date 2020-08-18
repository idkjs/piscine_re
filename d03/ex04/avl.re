type tree('a) =
  | Nil
  | Node('a, tree('a), tree('a));

/* ********************* previous exercise 03 ***************************** */

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

/* **************************** exercise 04 ************************************ */

let right_rotate = (tree: tree('a)) =>
  switch (tree) {
  | Nil => tree
  | [@implicit_arity] Node(v, Nil, r) => tree
  | [@implicit_arity] Node(v, [@implicit_arity] Node(vl, ll, lr), r) =>
    [@implicit_arity] Node(vl, ll, [@implicit_arity] Node(v, lr, r))
  };

let left_rotate = (tree: tree('a)) =>
  switch (tree) {
  | Nil => tree
  | [@implicit_arity] Node(v, l, Nil) => tree
  | [@implicit_arity] Node(v, l, [@implicit_arity] Node(vr, rl, rr)) =>
    [@implicit_arity] Node(vr, [@implicit_arity] Node(v, l, rl), rr)
  };

let left_right_rotate = (tree: tree('a)) =>
  switch (tree) {
  | Nil => tree
  | [@implicit_arity] Node(v, l, r) =>
    let new_right = right_rotate(r);
    left_rotate([@implicit_arity] Node(v, l, new_right));
  };

let right_left_rotate = (tree: tree('a)) =>
  switch (tree) {
  | Nil => tree
  | [@implicit_arity] Node(v, l, r) =>
    let new_left = left_rotate(l);
    right_rotate([@implicit_arity] Node(v, new_left, r));
  };

let rotate_node = node =>
  switch (node) {
  | [@implicit_arity] Node(v, l, r) when height(l) > height(r) =>
    switch (l) {
    | [@implicit_arity] Node(vl, ll, lr) when height(lr) > height(ll) =>
      right_left_rotate(node)
    | _ => right_rotate(node)
    | [@implicit_arity] Node(v, l, r) when height(r) > height(l) =>
      switch (r) {
      | [@implicit_arity] Node(vr, rl, rr) when height(rl) > height(rr) =>
        left_right_rotate(node)
      | _ => left_rotate(node)
      | _ => failwith("Error in rotate_node : Node is Nil or balanced")
      }
    }
  };

let insert_avl = (value, avl: tree('a)) =>
  if (!is_balanced(avl)) {
    failwith("Error: the tree passed as avl to insert_avl is not balanced");
  } else {
    let newTree = add_bst(value, avl);
    if (is_balanced(newTree)) {
      newTree;
    } else {
      let rec balancing_tree = current_node =>
        switch (current_node) {
        | [@implicit_arity] Node(v, l, r) when v < value =>
          let tree_right = balancing_tree(r);
          if (!is_balanced(tree_right)) {
            [@implicit_arity] Node(v, l, rotate_node(tree_right));
          } else {
            [@implicit_arity] Node(v, l, tree_right);
          };
        | [@implicit_arity] Node(v, l, r) when v > value =>
          let tree_left = balancing_tree(l);
          if (!is_balanced(tree_left)) {
            [@implicit_arity] Node(v, rotate_node(tree_left), r);
          } else {
            [@implicit_arity] Node(v, tree_left, r);
          };
        | [@implicit_arity] Node(v, l, r) when v == value => current_node
        | _ =>
          failwith("Error in balancing tree : no match in pattern matching")
        };
      balancing_tree(newTree);
    };
  };

let delete_avl = (value, avl: tree('a)) =>
  if (!is_balanced(avl)) {
    failwith("Error: the tree passed as avl to insert_avl is not balanced");
  } else {
    let newTree = delete_bst(value, avl);
    if (is_balanced(newTree)) {
      newTree;
    } else {
      let rec balancing_tree = current_node =>
        switch (current_node) {
        | [@implicit_arity] Node(v, l, r) when v < value =>
          let tree_right = balancing_tree(r);
          if (!is_balanced(tree_right)) {
            [@implicit_arity] Node(v, l, rotate_node(tree_right));
          } else {
            [@implicit_arity] Node(v, l, tree_right);
          };
        | [@implicit_arity] Node(v, l, r) when v > value =>
          let tree_left = balancing_tree(l);
          if (!is_balanced(tree_left)) {
            [@implicit_arity] Node(v, rotate_node(tree_left), r);
          } else {
            [@implicit_arity] Node(v, tree_left, r);
          };
        | [@implicit_arity] Node(v, Nil, Nil) => current_node
        | _ =>
          failwith("Error in balancing tree : no match in pattern matching")
        };
      balancing_tree(newTree);
    };
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
  let tree =
    [@implicit_arity]
    Node(
      10,
      [@implicit_arity] Node(5, Nil, Nil),
      [@implicit_arity] Node(12, Nil, Nil),
    );
  let tree1 =
    [@implicit_arity]
    Node(
      10,
      [@implicit_arity]
      Node(
        5,
        [@implicit_arity] Node(2, Nil, Nil),
        [@implicit_arity] Node(6, Nil, Nil),
      ),
      [@implicit_arity]
      Node(
        12,
        [@implicit_arity] Node(11, Nil, Nil),
        [@implicit_arity] Node(14, Nil, Nil),
      ),
    );
  let tree2 =
    [@implicit_arity]
    Node(
      10,
      [@implicit_arity]
      Node(
        5,
        [@implicit_arity] Node(2, Nil, Nil),
        [@implicit_arity] Node(8, [@implicit_arity] Node(6, Nil, Nil), Nil),
      ),
      [@implicit_arity]
      Node(
        16,
        [@implicit_arity]
        Node(
          13,
          [@implicit_arity] Node(11, Nil, Nil),
          [@implicit_arity] Node(14, Nil, Nil),
        ),
        [@implicit_arity] Node(18, Nil, Nil),
      ),
    );

  /* ******************** insertion in avl ********************** */
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree2);
  ignore(Graphics.read_key());

  let tree27 = insert_avl(7, tree2);
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree27);
  ignore(Graphics.read_key());

  let tree2715 = insert_avl(15, tree27);
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree2715);
  ignore(Graphics.read_key());

  let tree2715_14 = delete_avl(14, tree2715);
  Graphics.open_graph(" 2048x2048");
  draw_tree(tree2715_14);
  ignore(Graphics.read_key());
};

/* ******************** unit tests right_rotate ********************** */
/* Graphics.open_graph " 2048x2048" ;
   draw_tree tree ;
   ignore(Graphics.read_key ()) ;
   Graphics.open_graph " 2048x2048" ;
   draw_tree (right_rotate tree) ;
   ignore(Graphics.read_key ()) ;

   Graphics.open_graph " 2048x2048" ;
   draw_tree tree1 ;
   ignore(Graphics.read_key ()) ;
   Graphics.open_graph " 2048x2048" ;
   draw_tree (right_rotate tree1) ;
   ignore(Graphics.read_key ()) ;

   Graphics.open_graph " 2048x2048" ;
   draw_tree tree2 ;
   ignore(Graphics.read_key ()) ;
   Graphics.open_graph " 2048x2048" ;
   draw_tree (right_rotate tree2) ;
   ignore(Graphics.read_key ())  ; */

/* ******************** unit tests left_rotate ********************** */
/* Graphics.open_graph " 2048x2048" ;
   draw_tree tree ;
   ignore(Graphics.read_key ()) ;
   Graphics.open_graph " 2048x2048" ;
   draw_tree (left_rotate tree) ;
   ignore(Graphics.read_key ()) ;

   Graphics.open_graph " 2048x2048" ;
   draw_tree tree1 ;
   ignore(Graphics.read_key ()) ;
   Graphics.open_graph " 2048x2048" ;
   draw_tree (left_rotate tree1) ;
   ignore(Graphics.read_key ()) ;

   Graphics.open_graph " 2048x2048" ;
   draw_tree tree2 ;
   ignore(Graphics.read_key ()) ;
   Graphics.open_graph " 2048x2048" ;
   draw_tree (left_rotate tree2) ;
   ignore(Graphics.read_key ())   */

let () = main();
