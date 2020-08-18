type ft_ref('a) = {mutable contents: 'a};

let return = (x: 'a) => {contents: x};

let get = (x: ft_ref('a)) => x.contents;

let set = (x: ft_ref('a), y: 'a) => x.contents = y;

let bind = (x: ft_ref('a), f: 'a => ft_ref('b)) => f(get(x));

let () = {
  let my_ref = return(42);
  print_endline(string_of_int(get(my_ref)));
  ignore(set(my_ref, 9));
  print_endline(string_of_int(get(my_ref)));
  let my_transformed_ref =
    bind(my_ref, x => return(string_of_int(x * x) ++ " Huh?!"));
  print_endline(get(my_transformed_ref));
  ignore(set(my_transformed_ref, "coucou"));
  print_endline(get(my_transformed_ref));
};
