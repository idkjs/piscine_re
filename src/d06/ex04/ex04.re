module type VAL = {
  type t;
  let add: (t, t) => t;
  let mul: (t, t) => t;
};

module type EVALEXPR = {
  type t;
  type expr =
    | Value(t)
    | Add(expr, expr)
    | Mul(expr, expr);
  let eval: expr => t;
};

module type MAKEEVALEXPR =
  (ArithmeticModule: VAL) => EVALEXPR with type t = ArithmeticModule.t;

module MakeEvalExpr: MAKEEVALEXPR =
  (ArithmeticModule: VAL) => {
    type t = ArithmeticModule.t;
    type expr =
      | Value(t)
      | Add(expr, expr)
      | Mul(expr, expr);
    let rec eval = e =>
      switch (e) {
      | Value(x) => x
      | [@implicit_arity] Add(x, y) =>
        ArithmeticModule.add(eval(x), eval(y))
      | [@implicit_arity] Mul(x, y) =>
        ArithmeticModule.mul(eval(x), eval(y))
      };
  };

module IntVal: VAL with type t = int = {
  type t = int;
  let add = (+);
  let mul = ( * );
};

module FloatVal: VAL with type t = float = {
  type t = float;
  let add = (+.);
  let mul = ( *. );
};

module StringVal: VAL with type t = string = {
  type t = string;
  let add = (s1, s2) =>
    if (String.length(s1) > String.length(s2)) {
      s1;
    } else {
      s2;
    };
  let mul = (++);
};

module IntEvalExpr: EVALEXPR with type t = IntVal.t = MakeEvalExpr(IntVal);
module FloatEvalExpr: EVALEXPR with type t = FloatVal.t =
  MakeEvalExpr(FloatVal);
module StringEvalExpr: EVALEXPR with type t = StringVal.t =
  MakeEvalExpr(StringVal);

let ie =
  [@implicit_arity]
  IntEvalExpr.Add(IntEvalExpr.Value(40), IntEvalExpr.Value(2));
let fe =
  [@implicit_arity]
  FloatEvalExpr.Add(FloatEvalExpr.Value(41.5), FloatEvalExpr.Value(0.92));
let se =
  [@implicit_arity]
  StringEvalExpr.Mul(
    StringEvalExpr.Value("very "),
    [@implicit_arity]
    StringEvalExpr.Add(
      StringEvalExpr.Value("very long"),
      StringEvalExpr.Value("short"),
    ),
  );

let () = Printf.printf("Res = %d\n", IntEvalExpr.eval(ie));
let () = Printf.printf("Res = %f\n", FloatEvalExpr.eval(fe));
let () = Printf.printf("Res = %s\n", StringEvalExpr.eval(se));
