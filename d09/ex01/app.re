module App: {
  type project = (string, string, int);
  let zero: project;
  let combine: (project, project) => project;
  let fail: project => project;
  let success: project => project;
} = {
  type project = (string, string, int);
  let zero = ("", "", 0);
  let combine = ((name1, status1, g1), (name2, status2, g2)) => {
    let avge = (g1 + g2) / 2;
    let status =
      if (avge >= 80) {
        "succeed";
      } else {
        "failed";
      };
    (name1 ++ name2, status, avge);
  };
  let fail = ((n, _, _)) => (n, "failed", 0);
  let success = ((n, _, _)) => (n, "succeed", 80);
};
