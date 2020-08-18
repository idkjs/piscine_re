let rdString = () => {
  Random.self_init();
  let len = Random.int(12);
  let rec loop = (i: int, res: string) =>
    switch (i) {
    | x when x == len => res
    | _ => loop(i + 1, res ++ Dalek.charGenerator())
    };
  loop(0, "");
};

let () = {
  Random.self_init();
  let rec createListPeople = (i: int, res: list(People.people)) =>
    switch (i) {
    | 0 => res
    | _ =>
      createListPeople(i - 1, [(new People.people)(rdString()), ...res])
    };

  let rec createListDoctors =
          (
            i: int,
            peopleList: list(People.people),
            res: list(Doctor.doctor),
          ) =>
    switch (i) {
    | 0 => res
    | _ =>
      createListDoctors(
        i - 1,
        peopleList,
        [
          (new Doctor.doctor)(
            rdString(),
            Random.int(100),
            List.nth(peopleList, Random.int(List.length(peopleList))),
          ),
          ...res,
        ],
      )
    };

  let rec createListDaleks = (i: int, res) =>
    switch (i) {
    | 0 => res
    | _ => createListDaleks(i - 1, [new Dalek.dalek, ...res])
    };

  let lstPeople = createListPeople(Random.int(15) + 3, []);
  let lstDoctors = createListDoctors(Random.int(15) + 3, lstPeople, []);
  let lstDaleks = createListDaleks(Random.int(15) + 3, []);
  let armyPeople = (new Army.army)(lstPeople);
  let armyDoctors = (new Army.army)(lstDoctors);
  let armyDaleks = (new Army.army)(lstDaleks);
  let g = (new Galifrey.galifrey)(armyDaleks, armyDoctors, armyPeople);
  g#do_time_war;
};
