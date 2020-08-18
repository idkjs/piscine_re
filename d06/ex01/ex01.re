module StringWithHash = {
  include String;
  let hash = s => {
    let rec hash_aux = (i, h) =>
      switch (i) {
      | len when len == String.length(s) => h
      | _ => hash_aux(i + 1, int_of_char(s.[i]) + h lsl 6 + h lsl 16 - h)
      };
    hash_aux(0, 0);
  };
};

module StringHashtbl = Hashtbl.Make(StringWithHash);

let () = {
  let ht = StringHashtbl.create(5);
  let values = ["Hello", "world", "42", "Ocaml", "H"];
  let pairs = List.map(s => (s, String.length(s)), values);
  List.iter(((k, v)) => StringHashtbl.add(ht, k, v), pairs);
  StringHashtbl.iter(
    (k, v) => Printf.printf("k = \"%s\", v = %d\n", k, v),
    ht,
  );
};

/* sdbm hash algorithm : */
/* http://www.cse.yorku.ca/~oz/hash.html#:~:text=sdbm,hashing%20function%20with%20good%20distribution. */
