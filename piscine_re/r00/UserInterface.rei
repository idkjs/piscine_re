module Output: {
  let welcomeMessage: unit => string;
  let turnToPlay: Board.Cell.symbol => string;
  let gridFinished: (int, Board.Cell.symbol) => string;
  let playerWinGame: Board.Cell.symbol => string;
};

module Input: {
  let welcomeResponse: unit => string;
  let userChoice:
    (Board.BigBoard.t, Board.BoardCell.t, Board.Cell.symbol) => Board.Cell.t;
};
