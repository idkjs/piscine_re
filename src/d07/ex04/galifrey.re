class galifrey (daleks, doctors, people) = {
  as self;
  val _dalek_army: Army.army(Dalek.dalek) = daleks;
  val _doctor_army: Army.army(Doctor.doctor) = doctors;
  val _people_army: Army.army(People.people) = people;
  pub do_time_war =
    switch (
      _dalek_army#check_alive,
      _doctor_army#check_alive,
      _people_army#check_alive,
    ) {
    | (false, false, false) =>
      print_endline("Enf of the war. Everyone is DEAD!")
    | (true, false, false) =>
      print_endline("Only dalek left alive.");
      _dalek_army#get_fighter(0)#talk;
    | (false, true, false) =>
      print_endline("Only doctor left alive. ");
      let d = _doctor_army#get_fighter(0);
      d#talk;
      d#travel_in_time(2020, 2100);
    | (false, false, true) => print_endline("Only people left alive.")
    | (true, false, true) =>
      print_endline("Daleks and People are alive. Fight is on.");
      _dalek_army#get_fighter(0)#exterminate(_people_army#get_fighter(0));
      _people_army#delete;
      let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
      war#do_time_war;
    | (false, true, true) =>
      print_endline("People and Doctor are alive. Peace.");
      _people_army#get_fighter(0)#talk;
      _doctor_army#get_fighter(0)#talk;
    | (true, true, false) =>
      switch (Random.int(4)) {
      | 0 =>
        _doctor_army#get_fighter(0)#use_sonic_screwdriver;
        _dalek_army#get_fighter(0)#die;
        _dalek_army#delete;
        let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
        war#do_time_war;
      | 1 =>
        _doctor_army#get_fighter(0)#travel_in_time(10, 10);
        _dalek_army#get_fighter(0)#die;
        _dalek_army#delete;
        let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
        war#do_time_war;
      | 2 =>
        _dalek_army#get_fighter(0)#talk;
        let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
        war#do_time_war;
      | 3 =>
        _doctor_army#get_fighter(0)#talk;
        let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
        war#do_time_war;
      | _ => print_endline("WAR IS OVER.")
      }
    | (true, true, true) =>
      switch (Random.int(5)) {
      | 0 =>
        _people_army#get_fighter(0)#talk;
        _doctor_army#get_fighter(0)#talk;
        _dalek_army#get_fighter(0)#talk;
        _dalek_army#get_fighter(0)#exterminate(_people_army#get_fighter(0));
        _people_army#delete;
        let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
        war#do_time_war;
      | 1 =>
        _doctor_army#get_fighter(0)#use_sonic_screwdriver;
        _dalek_army#get_fighter(0)#die;
        _dalek_army#delete;
        let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
        war#do_time_war;
      | 2 =>
        _dalek_army#get_fighter(0)#talk;
        let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
        war#do_time_war;
      | 3 =>
        _doctor_army#get_fighter(0)#travel_in_time(10, 10);
        _dalek_army#get_fighter(0)#die;
        _dalek_army#delete;
        let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
        war#do_time_war;
      | 4 =>
        _doctor_army#get_fighter(0)#talk;
        let war = (new galifrey)(_dalek_army, _doctor_army, _people_army);
        war#do_time_war;
      | _ => print_endline("WAR IS OVER.")
      }
    };
};
