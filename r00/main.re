let main = () => {
  print_endline(UserInterface.Output.welcomeMessage());
  let player = Board.Cell.stringToSym(UserInterface.Input.welcomeResponse());
  let boardGame = Board.BigBoard.emptyBigBoard();
  let macroBoardInit = Board.BoardCell.emptyBoardCell();

  let rec game =
          (
            board: Board.BigBoard.t,
            macroBoard: Board.BoardCell.t,
            currPlayer: Board.Cell.symbol,
          ) => {
    print_endline(Board.BigBoard.bigBoardToString(board));
    print_endline(UserInterface.Output.turnToPlay(currPlayer));
    let cell = UserInterface.Input.userChoice(board, macroBoard, currPlayer);
    let newBoard = Board.BigBoard.insertCell(cell, board);
    switch (Backend.Calc.isGridFinished(cell, newBoard)) {
    | x when x == (-1) =>
      game(newBoard, macroBoard, Backend.Utility.switchUser(currPlayer))
    | x =>
      print_endline(UserInterface.Output.gridFinished(x, currPlayer));
      let newMacroBoard =
        Board.BoardCell.fillMacroBoard(macroBoard, x, currPlayer);
      let modifiedBoard =
        Board.BigBoard.insertBoardCell(
          Board.BoardCell.winningBoard(currPlayer),
          x,
          newBoard,
        );
      if (Backend.Calc.isGameFinished(newMacroBoard, currPlayer)) {
        print_endline(UserInterface.Output.playerWinGame(currPlayer));
        print_endline(Board.BigBoard.bigBoardToString(modifiedBoard));
      } else {
        game(
          modifiedBoard,
          newMacroBoard,
          Backend.Utility.switchUser(currPlayer),
        );
      };
    };
  };

  game(boardGame, macroBoardInit, player);
};

let () = main();
