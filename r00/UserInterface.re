module Output: {
  let welcomeMessage: unit => string;
  let turnToPlay: Board.Cell.symbol => string;
  let gridFinished: (int, Board.Cell.symbol) => string;
  let playerWinGame: Board.Cell.symbol => string;
} = {
  let welcomeMessage = () =>
    Printf.sprintf(
      "Hello and welcome to our Super Tictactoe!\nPlease choose which player is starting (X or O) :",
    );

  let turnToPlay = (player: Board.Cell.symbol) =>
    Printf.sprintf("%s's turn to play.", Board.Cell.symToString(player));

  let gridFinished = (x, s) =>
    Printf.sprintf("%s wins grid %d!", Board.Cell.symToString(s), x + 1);

  let playerWinGame = s =>
    Printf.sprintf("%s wins the game!", Board.Cell.symToString(s));
};

module Input: {
  let welcomeResponse: unit => string;
  let userChoice:
    (Board.BigBoard.t, Board.BoardCell.t, Board.Cell.symbol) => Board.Cell.t;
} = {
  let rec welcomeResponse = () => {
    let resp = read_line();
    let len = String.length(resp);
    switch (len) {
    | x when x != 1 =>
      print_endline(
        "Illegal format : Please choose which player is starting (X or O) :",
      );
      welcomeResponse();
    | _ =>
      switch (resp) {
      | c when c == "X" || c == "O" => resp
      | _ =>
        print_endline(
          "Unavailable choice : Please choose which player is starting (X or O) :",
        );
        welcomeResponse();
      }
    };
  };

  let rec userChoice =
          (
            board: Board.BigBoard.t,
            macro: Board.BoardCell.t,
            player: Board.Cell.symbol,
          ) => {
    let lenBoard = Board.BigBoard.boardLength(board);
    let resp = read_line();
    let inputList = String.split_on_char(' ', resp);
    switch (List.length(inputList)) {
    | x when x != 2 =>
      print_endline("Incorrect format.");
      userChoice(board, macro, player);
    | _ =>
      let first = List.hd(inputList);
      let second = List.nth(inputList, 1);
      let choice = (
        Backend.Utility.safeConvToInt(first),
        Backend.Utility.safeConvToInt(second),
      );
      switch (choice) {
      | (i, j) when i == None || j == None =>
        print_endline("Incorrect format.");
        userChoice(board, macro, player);
      | _ =>
        let i = Backend.Utility.getValueIntOption(fst(choice));
        let j = Backend.Utility.getValueIntOption(snd(choice));
        let coor = Backend.Utility.setCoor(i, j);
        if (i <= 0 || i > lenBoard || j <= 0 || j > lenBoard) {
          print_endline("Illegal move.");
          userChoice(board, macro, player);
        } else if (Board.BigBoard.isCellEmpty(coor, board) == false
                   || Board.BigBoard.isBoardCellAvail(coor, board, macro)
                   == false) {
          print_endline("Illegal move.");
          userChoice(board, macro, player);
        } else {
          Board.Cell.newCellPoint(coor, player);
        };
      };
    };
  };
};
